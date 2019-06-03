package moscow.ptnl.contingent.area.entity.nsi;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value = "org.hibernate.jpamodelgen.JPAMetaModelEntityProcessor")
@StaticMetamodel(AreaType.class)
public abstract class AreaType_ {

	public static volatile SingularAttribute<AreaType, Long> code;
	public static volatile SingularAttribute<AreaType, String> gender;
	public static volatile SingularAttribute<AreaType, Integer> ageMax;
	public static volatile SingularAttribute<AreaType, Integer> ageMMax;
	public static volatile SingularAttribute<AreaType, Boolean> archive;
	public static volatile SingularAttribute<AreaType, Integer> ageWMin;
	public static volatile SingularAttribute<AreaType, Integer> ageMin;
	public static volatile SingularAttribute<AreaType, AreaTypesClass> classAreaType;
	public static volatile SingularAttribute<AreaType, Integer> ageMMin;
	public static volatile SingularAttribute<AreaType, String> name;
	public static volatile SingularAttribute<AreaType, AreaTypesKind> kindAreaType;
	public static volatile SingularAttribute<AreaType, Specialization> specialization;
	public static volatile SingularAttribute<AreaType, PrimaryAreaTypeAttributes> attributes;
	public static volatile SingularAttribute<AreaType, Integer> ageWMax;

}

