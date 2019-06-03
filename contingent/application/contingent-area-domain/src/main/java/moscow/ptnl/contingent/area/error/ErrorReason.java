package moscow.ptnl.contingent.area.error;

public interface ErrorReason {
    /**
      * Вернуть описание ошибки
     * @return описание ошибки
     */
    String getDescription();
    String getCode();
}
