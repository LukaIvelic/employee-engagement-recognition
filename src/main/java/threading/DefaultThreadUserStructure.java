package threading;

public interface DefaultThreadUserStructure {
    default String returnUser(){
        return Thread.currentThread().getName();
    }
    default Boolean returnUseState(){
        return true;
    }
}