package controllers.login.enums;

public enum LoginStatus {
    ADMIN,
    USER,
    NOT_LOGGED_IN;

    @Override
    public String toString() {
        return this.name().toLowerCase();
    }
}
