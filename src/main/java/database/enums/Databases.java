package database.enums;

public enum Databases {
    EMPLOYEE_ENGAGEMENT_RECOGNITION,
    ADMIN;

    @Override
    public String toString(){
        return name().toLowerCase();
    }

    public enum Collections{
        EMPLOYEE,
        ENGAGEMENT;

        @Override
        public String toString(){
            return name().toLowerCase();
        }
    }
}