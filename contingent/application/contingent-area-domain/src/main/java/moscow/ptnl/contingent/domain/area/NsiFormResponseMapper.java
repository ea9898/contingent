package moscow.ptnl.contingent.domain.area;

import org.w3c.dom.Document;

public interface NsiFormResponseMapper {
    void transformAndMergeEntity(Document document, Object entityObj) throws IllegalAccessException;
}
