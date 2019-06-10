package moscow.ptnl.contingent.domain.history.meta;

/**
 * Описывающий методы класса конвертера значения журналируемого поля.
 * Реализация конвертера должна иметь безаргументный конструктор.
 * 
 * @author m.kachalov
 */
public interface FieldConverter {
    
    /**
     * Правило сравнения двух значений на эквивалентность.
     * Применяется для сравнения "старого" и "нового" значения поля.
     * 
     * @param value1
     * @param value2
     * @return 
     */
    boolean equals(Object value1, Object value2);
    
    /**
     * Формирует строковое представление значения поля.
     * 
     * @param value значение поля
     * @return 
     */
    String toString(Object value);
    
}
