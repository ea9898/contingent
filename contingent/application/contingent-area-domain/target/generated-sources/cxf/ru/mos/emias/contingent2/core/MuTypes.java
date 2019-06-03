
package ru.mos.emias.contingent2.core;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Перечень типов МУ.
 *                 Обязательно, если переданы типы участков для добавления (primaryAreaTypesAdd). При этом:
 *                 • если участок принадлежит филиалу, то передается тип филиала;
 *                 • если участок принадлежит МО в целом (филиал отсутствует), то в данной структуре должны передаваться типы всех МУ, принадлежащих данной МО
 *             
 * 
 * <p>Java class for MuTypes complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="MuTypes"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="muType" type="{http://emias.mos.ru/contingent2/core/v1/}MuType" maxOccurs="unbounded"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "MuTypes", propOrder = {
    "muTypes"
})
public class MuTypes
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    @XmlElement(name = "muType", required = true)
    protected List<MuType> muTypes;

    /**
     * Gets the value of the muTypes property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the muTypes property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getMuTypes().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link MuType }
     * 
     * 
     */
    public List<MuType> getMuTypes() {
        if (muTypes == null) {
            muTypes = new ArrayList<MuType>();
        }
        return this.muTypes;
    }

}
