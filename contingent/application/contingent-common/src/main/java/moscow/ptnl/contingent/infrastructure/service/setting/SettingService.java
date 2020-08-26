package moscow.ptnl.contingent.infrastructure.service.setting;

import java.util.List;

public interface SettingService {
    
    String Par1 = "max_addresses_for_allocation";
    String Par2 = "max_addresses_for_del_allocation";
    String Par3 = "max_page_size";
    String Par4 = "synchronize_k2_and_k1";
    String Par5 = "paging_default_page_number";
    String Par6 = "paging_default_page_size";
    String Par20 = "exception_specialization_codes";
    /** Глобальная настройка для отключения слушателей всех топиков ЕСУ. */
    String TOPICS_CONSUMERS_RUN_MODE = "topics.consumers.run.mode";
    /** Настройки ограничения доступа к методам web-сервиса. */
    String SERVICES_SECURITY_SETTINGS = "services.security.settings";
    /** Максимально допустимое количество адресных объектов для разового обновления из АР СУ НСИ. */
    String UPDATE_ADDRESS_BY_GLOBAL_ID_MAXCOUNT = "updateAddressByGlobalId.maxcount";
    /** Максимально допустимое количество адресных объектов для разового обновления из АР СУ НСИ. */
    String UPDATE_ADDRESS_BY_GLOBAL_ID_THREADS = "updateAddressByGlobalId.threads";
    /** Максимально допустимое количество ИД участков для поиска во входных параметрах метода. */
    String MAX_AREA_IDS_FOR_SEARCH = "max_areas_for_search";
    /** Включение/отключение режима отправки сообщений в ЕСУ из ESU_OUTPUT. */
    String PAR_30 = "esu_output_sending_enabled";
    /** Количество календарных дней хранения записей в ESU_INPUT. */
    String PAR_21 = "esu_input_period_keeping";
    /** Количество календарных дней хранения записей в ESU_OUTPUT. */
    String PAR_22 = "esu_output_period_keeping";
    /** Удалять из ESU_INPUT только успешно обработанные сообщения (0-отключен, 1-включен). */
    String PAR_23 = "esu_input_delete_success";
    /** Удалять из ESU_OUTPUT только успешно обработанные сообщения (0-отключен, 1-включен). */
    String PAR_24 = "esu_output_delete_success";
    /** Временной интервал запуска триггера очистки ESU_INPUT (HH:mm-HH:mm, например 00:30-04:00). */
    String PAR_25 = "esu_input_trigger_clean_start_interval";
    /** Частота запуска триггера очистки ESU_INPUT (минуты), например: 1440. */
    String PAR_26 = "esu_input_trigger_clean_start_period";
    /** Временной интервал запуска триггера очистки ESU_OUTPUT (HH:mm-HH:mm, например 00:30-04:00). */
    String PAR_27 = "esu_output_trigger_clean_start_interval";
    /** Частота запуска триггера очистки ESU_OUTPUT (минуты). */
    String PAR_28 = "esu_output_trigger_clean_start_period";
    /** Не запускать триггеры. */
    String PAR_29 = "do_not_run_triggers";
    /** Типы участков, для которых включен поиск адресов по точному совпадению адреса. */
    String PAR_31 = "exact_match_address_area_types";
    /** Включение поиска адресов по точному совпадению адреса для типов участков в exact_match_address_area_types. */
    String PAR_32 = "exact_match_address_enabled";

    /**
     * Свойство извлекается из кэша.
     * 
     * @param <T>
     * @param propertyName
     * @return 
     */
    <T> T getSettingProperty(String propertyName);
    
    <T> T getSettingProperty(String propertyName, boolean refesh);

    Long getPar1();

    Long getPar2();

    Long getPar3();

    List<Long> getPar20();

    Boolean getPar4();

    Integer getPar5();

    Integer getPar6();

    List<Long> par31();
}
