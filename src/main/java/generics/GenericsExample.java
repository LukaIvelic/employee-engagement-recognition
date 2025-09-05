package generics;

public class GenericsExample<V, T> {
    private V first;
    private T second;

    public GenericsExample(V first, T second) {
        setFirst(first);
        setSecond(second);
    }

    public V getFirst() {
        return first;
    }
    public void setFirst(V first) {
        this.first = first;
    }

    public T getSecond() {
        return second;
    }
    public void setSecond(T second) {
        this.second = second;
    }

    @Override
    public String toString() {
        System.out.println(getSecond().toString()+getFirst().toString());
        return "Pair [first=" + first + ", second=" + second + "]";
    }
}
