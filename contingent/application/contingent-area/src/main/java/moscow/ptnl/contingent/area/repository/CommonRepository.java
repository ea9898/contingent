package moscow.ptnl.contingent.area.repository;

import java.io.Serializable;

import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.NoRepositoryBean;

@NoRepositoryBean
public interface CommonRepository<T, ID extends Serializable> extends CrudRepository<T, ID > {


}
