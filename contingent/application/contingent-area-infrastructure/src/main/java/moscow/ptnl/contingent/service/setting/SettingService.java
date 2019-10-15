package moscow.ptnl.contingent.service.setting;

public interface SettingService {
    
    String Par1 = "max_addresses_for_allocation";
    String Par2 = "max_addresses_for_del_allocation";
    String Par3 = "max_page_size";
    String Par4 = "synchronize_k2_and_k1";
    String Par5 = "paging_default_page_number";
    String Par6 = "paging_default_page_size";
    /** Глобальная настройка для отключения слушателей всех топиков ЕСУ */
    String TOPICS_CONSUMERS_RUN_MODE = "topics.consumers.run.mode";
    String SERVICES_SECURITY_SETTINGS = "services.security.settings";
    
    <T> T getSettingProperty(String propertyName);

    Long getPar1();

    Long getPar2();

    Long getPar3();

    Boolean getPar4();

    Integer getPar5();

    Integer getPar6();
}
