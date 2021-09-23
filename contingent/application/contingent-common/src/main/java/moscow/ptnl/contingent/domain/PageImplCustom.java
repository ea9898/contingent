package moscow.ptnl.contingent.domain;

import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

import java.util.List;

public class PageImplCustom<T> extends PageImpl<T> {

    private final long totalMain;

    public PageImplCustom(List<T> content, Pageable pageable, long total) {
        super(content, pageable, total);
        this.totalMain = total;
    }

    public PageImplCustom(List<T> content) {
        super(content);
        this.totalMain = content.size();
    }

    public long getTotalElements() {
        return this.totalMain;
    }
}
