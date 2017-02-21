import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSet implements State {
    private volatile AtomicIntegerArray value;
    private byte maxval;

    GetNSet(byte[] v) {
	int len = v.length;
	value = new AtomicIntegerArray(v.length);
	for (int i = 0; i < len; i++)
	    value.set(i, (int) (v[i])); 
        maxval = 127;
    }

    GetNSet(byte[] v, byte m) {
	int len = v.length;
	value = new AtomicIntegerArray(v.length);
	for (int i = 0; i < len; i++)
	    value.set(i, (int) (v[i]));  
        maxval = m;
    }

    public int size() { return value.length(); }

    public byte[] current() {
	int len = value.length();
	byte[] v = new byte[value.length()];
	for (int i = 0; i < len; i++)
	    v[i] = (byte) (value.get(i));
	return v;
    }

    public boolean swap(int i, int j) {
	if (value.get(i) <= 0 || value.get(i) >= maxval) {
	    return false;
	}
	value.set(i, (value.get(i)-1));
	value.set(j, (value.get(j)+1));
	return true;
    }
}
