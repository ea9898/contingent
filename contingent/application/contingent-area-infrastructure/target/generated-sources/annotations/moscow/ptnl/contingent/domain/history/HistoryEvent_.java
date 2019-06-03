package moscow.ptnl.contingent.domain.history;

import java.time.LocalDateTime;
import javax.annotation.Generated;
import javax.persistence.metamodel.SetAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value = "org.hibernate.jpamodelgen.JPAMetaModelEntityProcessor")
@StaticMetamodel(HistoryEvent.class)
public abstract class HistoryEvent_ {

	public static volatile SingularAttribute<HistoryEvent, Long> eventId;
	public static volatile SetAttribute<HistoryEvent, HistoryEventValue> values;
	public static volatile SingularAttribute<HistoryEvent, String> methodName;
	public static volatile SingularAttribute<HistoryEvent, Long> lpuId;
	public static volatile SingularAttribute<HistoryEvent, ServiceName> serviceName;
	public static volatile SingularAttribute<HistoryEvent, Long> jobInfoId;
	public static volatile SingularAttribute<HistoryEvent, String> userLogin;
	public static volatile SingularAttribute<HistoryEvent, Long> accountId;
	public static volatile SingularAttribute<HistoryEvent, String> sourceType;
	public static volatile SingularAttribute<HistoryEvent, LocalDateTime> changeDate;
	public static volatile SingularAttribute<HistoryEvent, JournalHistoryTable> typeId;
	public static volatile SingularAttribute<HistoryEvent, Long> notificationId;
	public static volatile SingularAttribute<HistoryEvent, Long> id;
	public static volatile SingularAttribute<HistoryEvent, Long> userRoleId;
	public static volatile SingularAttribute<HistoryEvent, Long> objectId;

}

