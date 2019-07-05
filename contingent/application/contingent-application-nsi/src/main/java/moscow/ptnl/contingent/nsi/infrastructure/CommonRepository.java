package moscow.ptnl.contingent.nsi.infrastructure;

import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.NoRepositoryBean;

import java.io.Serializable;

@NoRepositoryBean
public interface CommonRepository<T, ID extends Serializable> extends CrudRepository<T, ID > {


}
