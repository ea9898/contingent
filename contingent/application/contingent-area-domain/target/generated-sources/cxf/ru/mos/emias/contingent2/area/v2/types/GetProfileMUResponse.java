
package ru.mos.emias.contingent2.area.v2.types;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import ru.mos.emias.contingent2.core.v2.AreaTypeShort;


/**
 * <p>Java class for anonymous complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="profileAreaType" type="{http://emias.mos.ru/contingent2/core/v2/}AreaTypeShort" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "profileAreaTypes"
})
@XmlRootElement(name = "getProfileMUResponse")
public class GetProfileMUResponse
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    @XmlElement(name = "profileAreaType")
    protected List<AreaTypeShort> profileAreaTypes;

    /**
     * Gets the value of the profileAreaTypes property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the profileAreaTypes property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getProfileAreaTypes().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link AreaTypeShort }
     * 
     * 
     */
    public List<AreaTypeShort> getProfileAreaTypes() {
        if (profileAreaTypes == null) {
            profileAreaTypes = new ArrayList<AreaTypeShort>();
        }
        return this.profileAreaTypes;
    }

}
