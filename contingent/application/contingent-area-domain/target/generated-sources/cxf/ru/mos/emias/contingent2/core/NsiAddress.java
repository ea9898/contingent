
package ru.mos.emias.contingent2.core;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for NsiAddress complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="NsiAddress"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="levelAddress" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="globalId" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "NsiAddress", propOrder = {
    "levelAddress",
    "globalId"
})
public class NsiAddress
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    protected int levelAddress;
    protected long globalId;

    /**
     * Gets the value of the levelAddress property.
     * 
     */
    public int getLevelAddress() {
        return levelAddress;
    }

    /**
     * Sets the value of the levelAddress property.
     * 
     */
    public void setLevelAddress(int value) {
        this.levelAddress = value;
    }

    /**
     * Gets the value of the globalId property.
     * 
     */
    public long getGlobalId() {
        return globalId;
    }

    /**
     * Sets the value of the globalId property.
     * 
     */
    public void setGlobalId(long value) {
        this.globalId = value;
    }

}
