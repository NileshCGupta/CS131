import java.util.concurrent.atomic.AtomicInteger;

class BetterSorry implements State {
    private AtomicInteger[] value;
    private byte maxval;

    BetterSorry(byte[] v) { fillArray(v); maxval = 127; }

    BetterSorry(byte[] v, byte m) { fillArray(v); maxval = m; }

    public void fillArray(byte[] v) {
	int len = v.length;
	value = new AtomicInteger[len];
	for (int i = 0; i < len; i++) {
	    value[i] = new AtomicInteger((int) v[i]);
	}
    }
    
    public int size() { return value.length; }

    public byte[] current() {
	int len = value.length;
	byte[] v = new byte[len];
	for (int i = 0; i < len; i++)
	    v[i] = (byte) (value[i].get());
	return v;
    }

    public boolean swap(int i, int j) {
	if (value[i].get() <= 0 || value[j].get() >= maxval) {
	    return false;
	}
	value[i].decrementAndGet();
	value[j].incrementAndGet();;
	return true;
    }
}
