package moscow.ptnl.contingent.area.entity.settings;

import java.time.LocalDateTime;
import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value = "org.hibernate.jpamodelgen.JPAMetaModelEntityProcessor")
@StaticMetamodel(Setting.class)
public abstract class Setting_ {

	public static volatile SingularAttribute<Setting, String> val;
	public static volatile SingularAttribute<Setting, String> name;
	public static volatile SingularAttribute<Setting, String> description;
	public static volatile SingularAttribute<Setting, LocalDateTime> lastChange;
	public static volatile SingularAttribute<Setting, Long> type;
	public static volatile SingularAttribute<Setting, String> value;

}

