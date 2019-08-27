package moscow.ptnl.contingent.repository;

import java.io.Serializable;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.NoRepositoryBean;

@NoRepositoryBean
public interface PagingAndSortingRepository<T, ID extends Serializable> extends CommonRepository<T, ID>, JpaSpecificationExecutor<T> {

	Iterable<T> findAll(Sort sort);

	Page<T> findAll(Pageable pageable);
	
	Page<T> findAll(Specification<T> spec, Pageable pageRequest);

	List<T> findAll(Specification<T> spec, Sort sort);
}
