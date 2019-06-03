package moscow.ptnl.contingent.domain.esu;

import java.time.LocalDateTime;
import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value = "org.hibernate.jpamodelgen.JPAMetaModelEntityProcessor")
@StaticMetamodel(EsuOutput.class)
public abstract class EsuOutput_ {

	public static volatile SingularAttribute<EsuOutput, Integer> partition;
	public static volatile SingularAttribute<EsuOutput, Long> offset;
	public static volatile SingularAttribute<EsuOutput, String> esuId;
	public static volatile SingularAttribute<EsuOutput, LocalDateTime> sentTime;
	public static volatile SingularAttribute<EsuOutput, String> topic;
	public static volatile SingularAttribute<EsuOutput, Long> id;
	public static volatile SingularAttribute<EsuOutput, String> message;
	public static volatile SingularAttribute<EsuOutput, Integer> status;

}

