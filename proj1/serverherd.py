import time
import logging
import sys
import json
import conf

from twisted.internet import reactor, protocol
from twisted.protocols.basic import LineReceiver
from twisted.python import log
from twisted.web.client import getPage
from twisted.application import service, internet

class ServerProtocol(LineReceiver):
    def __init__(self, factory):
        self.factory = factory

    def connectionMade(self):
        self.factory.connections += 1
        logging.info("Connection made. Total: {0}".format(self.factory.connections))

    def connectionLost(self, reason):
        self.factory.connections = self.factory.connections - 1
        logging.info("Connection lost. Current connections: {0}".format(self.factory.connections))

    def lineReceived(self, line):
        logging.info("Line received: {0}".format(line))
        words = line.split()

        if len(words) < 1:
          self.commandFailed(line)
          return

        if (words[0] == "IAMAT"):
          self.handle_IAMAT(line)
        elif (words[0] == "WHATSAT"):
          self.handle_WHATSAT(line)
        elif (words[0] == "AT"):
          self.handle_AT(line)
        else:
          self.commandFailed(line)
        return

    def commandFailed(self, line):
        logging.info("Invalid command: " + line)
        self.transport.write("? " + line + "\n")
        return

    def handle_IAMAT(self, line):
        try:
            words = line.split()

            ID = words[1]
            coordinates = words[2]
            timeOfMessage = words[3]        

            timeDiff = time.time() - float(timeOfMessage)

            message = "AT " + self.factory.name

            if timeDiff >= 0:
              message += " +" + str(timeDiff) + " " + ' '.join(words[1:])
            else:
              message += " " + str(timeDiff) + " " + ' '.join(words[1:])

            self.transport.write(message + "\n")
            logging.info("IAMAT Response: " + message)

            if (ID in self.factory.clients):
                if (timeOfMessage <= self.factory.clients[ID]["time"]):
                    logging.info("Duplicate or outdated AT info: " + line)
                    return
            self.factory.clients[ID] = {"message": message, "time": timeOfMessage}

            self.talk(message)

        except StandardError:
            self.commandFailed(line)

    def handle_AT(self, line):
        try:
            words = line.split()

            ID = words[3]
            timeOfMessage = words[5]
            sender = words[6]

            if (ID in self.factory.clients):
                if (timeOfMessage <= self.factory.clients[ID]["time"]):
                    logging.info("Outdated AT: " + line)
                    return


            self.factory.clients[ID] = {"message": ' '.join(words[:-1]), "time": timeOfMessage}

            logging.info("Changed {0} Status: {1}".format(ID, self.factory.clients[ID]["message"]))
            self.talk(self.factory.clients[ID]["message"], sender)
            return

        except StandardError:
            self.commandFailed(line)
            return

    def handle_WHATSAT(self, line):
        try:
            words = line.split()
            
            ID = words[1]
            radius = int(words[2])
            limit = int(words[3])

            if radius > 50 or limit > 20:
              self.commandFailed(line)
              return
         
            if not (ID in self.factory.clients):
              self.commandFailed(line)
              return

            msg = self.factory.clients[ID]["message"]
            coordinates = msg.split()[4]
            newcoordinates = coordinates.replace('+', ' +').replace('-', ' -').strip().replace(' ', ',').replace('+', '')
            url = "{0}location={1}&radius={2}&sensor=false&key={3}".format(conf.URL_PREFIX, newcoordinates, str(radius), conf.API_KEY)
            logging.info("Google places URL: {0}".format(url))
            response = getPage(url)
            response.addCallback(callback = lambda arg:(self.QueryCallBack(arg, msg, limit, url)))

        except StandardError:
            logging.error('WHATSAT request failure: ' + line)

    def QueryCallBack(self, response, msg, limit, url):
        msgObj = json.loads(response)
        results = msgObj["results"]
        msgObj["results"] = results[0:limit]
        returnMsg = "{0}\n{1}\n\n".format(msg, json.dumps(msgObj, indent=4))
        self.transport.write(returnMsg)
        logging.info("Responded to IAMAT with: " + msg + "; and Google Places query: " + url)


    def talk(self, line, sender = ''):
        msg = line + ' ' + self.factory.name
        for neighbor in conf.NEIGHBORS[self.factory.name]:
          if neighbor != sender:
            if neighbor in self.factory.connectedServers:
              self.factory.connectedServers[neighbor].sendAtMsg(msg)
              logging.info("Update sent from {0} to {1}".format(self.factory.name, neighbor))
            else:
              reactor.connectTCP(conf.SERVERS[neighbor]["ip"], conf.SERVERS[neighbor]["port"], ClientSetup(self.factory, neighbor, msg))
              logging.info("Update sent from {0} to {1}".format(self.factory.name, neighbor))
        return

class ServerSetup(protocol.ServerFactory):
    def __init__(self, name, port):
        self.name = name
        self.port = port
        self.connections = 0
        self.connectedServers = {}
        self.clients = {}

        self.logFile = self.name + ".log"
        logging.basicConfig(filename = self.logFile, level = logging.DEBUG, filemode = 'a', format='%(asctime)s %(message)s')
        logging.info('{0} server started at port {1}'.format(self.name, self.port))

    def buildProtocol(self, addr):
        return ServerProtocol(self)

    def terminate(self):
        logging.info("{0} server terminated".format(self.name))

class ClientProtocol(LineReceiver):
    def __init__ (self, factory):
        self.factory = factory 

    def connectionMade(self):
        self.factory.clientObj.connectedServers[self.factory.name] = self.factory
        logging.info("Connection from client: {0} to server: {1} established.".format(self.factory.clientObj.name, self.factory.name))
        self.sendLine(self.factory.msg)

    def connectionLost(self, reason):
        if self.factory.name in self.factory.clientObj.connectedServers:
            del self.factory.clientObj.connectedServers[self.factory.name]
            logging.info("Connection from client: {0} to server: {1} lost.".format(self.factory.clientObj.name, self.factory.name)) 
        return

class ClientSetup(protocol.ClientFactory):
    def __init__(self, clientObj, name, msg):
        self.clientObj = clientObj
        self.name = name
        self.msg = msg
        return

    def buildProtocol(self, addr):
        self.protocol = ClientProtocol(self)
        return self.protocol

    def sendAtMsg(self, msg):
        try:
          self.protocol.sendLine(msg)
        except StandardError:
          logging.error("Client message send error: " + msg)
        return
      
    def clientConnectionLost(self, connector, reason):
        if self.name in self.clientObj.connectedServers:
          del self.clientObj.connectedServers[self.name]
          logging.info("Connection from client: {0} to server: {1} lost.".format(self.clientObj.name, self.name))
        return

    def clientConnectionFailed(self, connector, reason):
        logging.info("Connection from client: {0} to server: {1} failed.".format(self.clientObj.name, self.name))
        return


def main():
    if (len(sys.argv) != 2):
        print "Error: Incorrect number of args"
        exit()

    name = sys.argv[1]
    if name in conf.SERVERS:
        factory = ServerSetup(name, conf.SERVERS[name]["port"])
        reactor.listenTCP(conf.SERVERS[name]["port"], factory)
        reactor.run()
    else:
        print "Error: server name not recognized"


if __name__ == '__main__':
    main()
