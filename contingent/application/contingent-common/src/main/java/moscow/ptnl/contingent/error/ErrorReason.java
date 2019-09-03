package moscow.ptnl.contingent.error;

public interface ErrorReason {
    /**
      * Вернуть описание ошибки
     * @return описание ошибки
     */
    String getDescription();
    String getCode();
}
