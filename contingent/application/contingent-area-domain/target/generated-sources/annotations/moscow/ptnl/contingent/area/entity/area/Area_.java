package moscow.ptnl.contingent.area.entity.area;

import java.time.LocalDateTime;
import javax.annotation.Generated;
import javax.persistence.metamodel.SetAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;

@Generated(value = "org.hibernate.jpamodelgen.JPAMetaModelEntityProcessor")
@StaticMetamodel(Area.class)
public abstract class Area_ {

	public static volatile SingularAttribute<Area, LocalDateTime> updateDate;
	public static volatile SingularAttribute<Area, Integer> ageMax;
	public static volatile SingularAttribute<Area, Integer> ageMMax;
	public static volatile SingularAttribute<Area, Boolean> autoAssignForAttach;
	public static volatile SingularAttribute<Area, String> description;
	public static volatile SingularAttribute<Area, Integer> ageWMin;
	public static volatile SingularAttribute<Area, Long> moId;
	public static volatile SingularAttribute<Area, Long> muId;
	public static volatile SingularAttribute<Area, Integer> number;
	public static volatile SingularAttribute<Area, Boolean> archived;
	public static volatile SingularAttribute<Area, Integer> ageMin;
	public static volatile SetAttribute<Area, AreaToAreaType> primaryAreaTypes;
	public static volatile SingularAttribute<Area, AreaType> areaType;
	public static volatile SingularAttribute<Area, Integer> ageMMin;
	public static volatile SetAttribute<Area, AreaAddress> areaAddresses;
	public static volatile SingularAttribute<Area, Long> id;
	public static volatile SingularAttribute<Area, Integer> ageWMax;
	public static volatile SetAttribute<Area, AreaMedicalEmployee> medicalEmployees;
	public static volatile SingularAttribute<Area, Boolean> attachByMedicalReason;
	public static volatile SingularAttribute<Area, LocalDateTime> createDate;

}

