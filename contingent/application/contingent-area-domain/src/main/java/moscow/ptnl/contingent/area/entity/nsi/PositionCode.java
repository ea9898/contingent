package moscow.ptnl.contingent.area.entity.nsi;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.Objects;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsi;

@Entity
@Table(name = "POSITION_CODE")
@Cacheable
@MapToNsi(table = NsiTablesEnum.POSITION_CODE)
public class PositionCode implements Serializable {

    private static final long serialVersionUID = 3663299049984440497L;

    @Id
    @Column(name = "GLOBAL_ID", unique = true, nullable = false)
    @MapToNsi("GLOBAL_ID")
    private Long globalId;

    @Size(max = 10)
    @Column(name = "NOM_TYPE", nullable = false)
    @MapToNsi("NOM_TYPE")
    private String nomType;

    @Column(name = "SERIAL_NUM", nullable = false)
    @MapToNsi("SERIAL_NUM")
    private Long serialNum;

    @Size(max = 100)
    @Column(name = "CODE", nullable = false)
    @MapToNsi
    private String code;

    @Size(max = 250)
    @Column(name = "CONSTANT_TITLE", nullable = false)
    @MapToNsi("CONSTANT_TITLE")
    private String constantTitle;

    public PositionCode() {}

    public PositionCode(String code) {
        this.code = code;
    }

    public PositionCode(Long globalId, String nomType, Long serialNum, String code, String constantTitle) {
        this.globalId = globalId;
        this.nomType = nomType;
        this.serialNum = serialNum;
        this.code = code;
        this.constantTitle = constantTitle;
    }

    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
    }

    public String getNomType() {
        return nomType;
    }

    public void setNomType(String nomType) {
        this.nomType = nomType;
    }

    public Long getSerialNum() {
        return serialNum;
    }

    public void setSerialNum(Long serialNum) {
        this.serialNum = serialNum;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getConstantTitle() {
        return constantTitle;
    }

    public void setConstantTitle(String constantTitle) {
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
