package moscow.ptnl.contingent.area.entity.area;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;
import moscow.ptnl.contingent.area.entity.nsi.AddressFormingElement;
import moscow.ptnl.contingent.area.entity.nsi.BuildingRegistry;

@Generated(value = "org.hibernate.jpamodelgen.JPAMetaModelEntityProcessor")
@StaticMetamodel(Addresses.class)
public abstract class Addresses_ {

	public static volatile SingularAttribute<Addresses, Integer> level;
	public static volatile SingularAttribute<Addresses, AddressFormingElement> addressFormingElement;
	public static volatile SingularAttribute<Addresses, Long> id;
	public static volatile SingularAttribute<Addresses, BuildingRegistry> buildingRegistry;

}

