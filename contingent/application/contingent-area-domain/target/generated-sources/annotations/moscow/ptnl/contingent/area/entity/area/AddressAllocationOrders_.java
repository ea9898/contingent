package moscow.ptnl.contingent.area.entity.area;

import java.time.LocalDate;
import java.time.LocalDateTime;
import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value = "org.hibernate.jpamodelgen.JPAMetaModelEntityProcessor")
@StaticMetamodel(AddressAllocationOrders.class)
public abstract class AddressAllocationOrders_ {

	public static volatile SingularAttribute<AddressAllocationOrders, LocalDate> date;
	public static volatile SingularAttribute<AddressAllocationOrders, String> number;
	public static volatile SingularAttribute<AddressAllocationOrders, Boolean> archived;
	public static volatile SingularAttribute<AddressAllocationOrders, LocalDateTime> updateDate;
	public static volatile SingularAttribute<AddressAllocationOrders, String> ouz;
	public static volatile SingularAttribute<AddressAllocationOrders, String> name;
	public static volatile SingularAttribute<AddressAllocationOrders, Long> id;
	public static volatile SingularAttribute<AddressAllocationOrders, LocalDateTime> createDate;

}

