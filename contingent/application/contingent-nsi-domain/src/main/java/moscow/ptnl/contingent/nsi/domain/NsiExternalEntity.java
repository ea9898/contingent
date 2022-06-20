package moscow.ptnl.contingent.nsi.domain;

import moscow.ptnl.contingent.domain.Keyable;

import java.time.LocalDateTime;

/**
 * Вещаем на сущности, которые обновляются из НСИ и имеют соответствующие поля
*/
public interface NsiExternalEntity extends Keyable {

    Long getGlobalId();

    LocalDateTime getUpdateDate();

    String getSource();

    void setUpdateDate(LocalDateTime updateDate);

    void setSource(String source);

    Boolean getArchived();

    void setArchived(Boolean archived);
}
