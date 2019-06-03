package moscow.ptnl.contingent.area.entity.area;

import java.time.LocalDate;
import java.time.LocalDateTime;
import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;

@Generated(value = "org.hibernate.jpamodelgen.JPAMetaModelEntityProcessor")
@StaticMetamodel(MoAddress.class)
public abstract class MoAddress_ {

	public static volatile SingularAttribute<MoAddress, LocalDateTime> updateDate;
	public static volatile SingularAttribute<MoAddress, Addresses> address;
	public static volatile SingularAttribute<MoAddress, LocalDate> endDate;
	public static volatile SingularAttribute<MoAddress, AreaType> areaType;
	public static volatile SingularAttribute<MoAddress, AddressAllocationOrders> addressRejectOrder;
	public static volatile SingularAttribute<MoAddress, Long> id;
	public static volatile SingularAttribute<MoAddress, Long> moId;
	public static volatile SingularAttribute<MoAddress, LocalDate> startDate;
	public static volatile SingularAttribute<MoAddress, AddressAllocationOrders> addressAllocationOrder;
	public static volatile SingularAttribute<MoAddress, LocalDateTime> createDate;

}

