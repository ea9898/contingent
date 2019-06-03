package moscow.ptnl.contingent.area.entity.area;

import java.time.LocalDate;
import java.time.LocalDateTime;
import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value = "org.hibernate.jpamodelgen.JPAMetaModelEntityProcessor")
@StaticMetamodel(AreaAddress.class)
public abstract class AreaAddress_ {

	public static volatile SingularAttribute<AreaAddress, Area> area;
	public static volatile SingularAttribute<AreaAddress, LocalDateTime> updateDate;
	public static volatile SingularAttribute<AreaAddress, Addresses> address;
	public static volatile SingularAttribute<AreaAddress, LocalDate> endDate;
	public static volatile SingularAttribute<AreaAddress, MoAddress> moAddress;
	public static volatile SingularAttribute<AreaAddress, Long> id;
	public static volatile SingularAttribute<AreaAddress, LocalDate> startDate;
	public static volatile SingularAttribute<AreaAddress, LocalDateTime> createDate;

}

