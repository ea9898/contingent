package moscow.ptnl.contingent.nsi.domain;

import java.time.LocalDateTime;

/**
 * Вещаем на сущности, которые обновляются из НСИ и имеют соответствующие поля
*/
public interface NsiExternalEntity {

    LocalDateTime getUpdateDate();

    String getSource();

    void setUpdateDate(LocalDateTime updateDate);

    void setSource(String source);
}
