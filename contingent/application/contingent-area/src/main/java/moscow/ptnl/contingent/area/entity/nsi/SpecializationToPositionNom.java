package moscow.ptnl.contingent.area.entity.nsi;

import moscow.ptnl.contingent.area.entity.converter.BooleanStrictIntegerConverter;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Objects;
import javax.persistence.IdClass;

@Entity
@IdClass(SpecializationToPositionNom.Key.class)
@Table(name = "SPECIALIZATION_TO_POSITION_NOM")
@Cacheable
public class SpecializationToPositionNom implements Serializable {

    private static final long serialVersionUID = -6560192129930420566L;

    @Id
    @JoinColumn(name = "SPECIALIZATION_ID", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private Specialization specialization;

    @Id
    @JoinColumn(name = "POSITION_NOM_ID", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private PositionNom positionNom;

    @Column(name = "ARCHIVED", nullable = false)
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private Boolean archived;
    
    public Key getKey() {
        return new Key(specialization, positionNom);
    }

    public Specialization getSpecialization() {
        return specialization;
    }

    public void setSpecialization(Specialization specialization) {
        this.specialization = specialization;
    }

    public PositionNom getPositionNom() {
        return positionNom;
    }

    public void setPositionNom(PositionNom positionNom) {
        this.positionNom = positionNom;
    }

    public Boolean getArchived() {
        return archived;
    }

    public void setArchived(Boolean archived) {
        this.archived = archived;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof SpecializationToPositionNom) {
            return ((SpecializationToPositionNom) obj).getKey().equals(this.getKey());
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return this.getKey().hashCode();
    }
    
    /**
     * Композитный ключ.
     */
    public static class Key implements Serializable {
        
        private Specialization specialization;
        private PositionNom positionNom;
        
        public Key(){}
        
        public Key(Specialization specialization, PositionNom positionNom) {
            this.specialization = specialization;
            this.positionNom = positionNom;
        }

        public Specialization getSpecialization() {
            return specialization;
        }

        public void setSpecialization(Specialization specialization) {
            this.specialization = specialization;
        }

        public PositionNom getPositionNom() {
            return positionNom;
        }

        public void setPositionNom(PositionNom positionNom) {
            this.positionNom = positionNom;
        }    
        
        @Override
        public boolean equals(Object obj) {
            if(this == obj)
                return true;
            if (obj != null && obj instanceof Key) {
                Key other = (Key) obj;
                if (other.getSpecialization() == null || other.getPositionNom() == null)
                    return false;
                return  other.getSpecialization().equals(this.getSpecialization()) && other.getPositionNom().equals(this.getPositionNom());
            }
            return false;
        }

        @Override
        public int hashCode() {
            return Objects.hash(this.getSpecialization(), this.getPositionNom());
        }
        
    }
}
