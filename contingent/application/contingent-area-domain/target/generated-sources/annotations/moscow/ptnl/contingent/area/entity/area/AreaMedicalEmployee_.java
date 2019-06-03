package moscow.ptnl.contingent.area.entity.area;

import java.time.LocalDate;
import java.time.LocalDateTime;
import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;
import moscow.ptnl.contingent.area.entity.nsi.PositionNomClinic;

@Generated(value = "org.hibernate.jpamodelgen.JPAMetaModelEntityProcessor")
@StaticMetamodel(AreaMedicalEmployee.class)
public abstract class AreaMedicalEmployee_ {

	public static volatile SingularAttribute<AreaMedicalEmployee, Area> area;
	public static volatile SingularAttribute<AreaMedicalEmployee, LocalDateTime> updateDate;
	public static volatile SingularAttribute<AreaMedicalEmployee, PositionNomClinic> positionNomClinic;
	public static volatile SingularAttribute<AreaMedicalEmployee, Long> subdivisionId;
	public static volatile SingularAttribute<AreaMedicalEmployee, String> ouz;
	public static volatile SingularAttribute<AreaMedicalEmployee, LocalDate> endDate;
	public static volatile SingularAttribute<AreaMedicalEmployee, Long> medicalEmployeeJobInfoId;
	public static volatile SingularAttribute<AreaMedicalEmployee, Long> id;
	public static volatile SingularAttribute<AreaMedicalEmployee, Boolean> replacement;
	public static volatile SingularAttribute<AreaMedicalEmployee, String> snils;
	public static volatile SingularAttribute<AreaMedicalEmployee, LocalDate> startDate;
	public static volatile SingularAttribute<AreaMedicalEmployee, LocalDateTime> createDate;

}

