package moscow.ptnl.contingent.area.entity.nsi;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDate;
import java.util.Objects;

@Entity
@Table(name = "POSITION_CODE")
@Cacheable
public class PositionCode implements Serializable {

    private static final long serialVersionUID = 3663299049984440497L;

    @Id
    @Column(name = "GLOBAL_ID", unique = true, nullable = false)
    private Long globalId;

    @Size(max = 10)
    @Column(name = "NOM_TYPE", nullable = false)
    private String nomType;

    @Column(name = "SERIAL_NUM", nullable = false)
    private Long serialNum;

    @Size(max = 100)
    @Column(name = "CODE", nullable = false)
    private String code;

    @Size(max = 250)
    @Column(name = "CONSTANT_TITLE", nullable = false)
    private String constantTitle;

    public PositionCode(Long globalId, String nomType, Long serialNum, String code, String constantTitle) {
        this.globalId = globalId;
        this.nomType = nomType;
        this.serialNum = serialNum;
        this.code = code;
        this.constantTitle = constantTitle;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        PositionCode that = (PositionCode) o;
        return Objects.equals(globalId, that.globalId) &&
                Objects.equals(nomType, that.nomType) &&
                Objects.equals(serialNum, that.serialNum) &&
                Objects.equals(code, that.code) &&
                Objects.equals(constantTitle, that.constantTitle);
    }

    @Override
    public int hashCode() {
        return Objects.hash(globalId, nomType, serialNum, code, constantTitle);
    }
}
