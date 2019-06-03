
package ru.mos.emias.system.v1.usercontext;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Контекст пользователя
 * 
 * <p>Java class for UserContext complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="UserContext"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="systemName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="userName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="userRoleId" type="{http://emias.mos.ru/system/v1/userContext/}ID"/&gt;
 *         &lt;element name="isUserRoleSystemWide" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/&gt;
 *         &lt;element name="userRights"&gt;
 *           &lt;complexType&gt;
 *             &lt;complexContent&gt;
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *                 &lt;sequence&gt;
 *                   &lt;element name="userRightId" type="{http://emias.mos.ru/system/v1/userContext/}ID" maxOccurs="unbounded" minOccurs="0"/&gt;
 *                 &lt;/sequence&gt;
 *               &lt;/restriction&gt;
 *             &lt;/complexContent&gt;
 *           &lt;/complexType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="jobExecutionId" type="{http://emias.mos.ru/system/v1/userContext/}ID"/&gt;
 *         &lt;element name="hostIp" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="hostName" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "UserContext", propOrder = {
    "systemName",
    "userName",
    "userRoleId",
    "isUserRoleSystemWide",
    "userRights",
    "jobExecutionId",
    "hostIp",
    "hostName"
})
@XmlRootElement(name = "userContext")
public class UserContext
    implements Serializable
{

    private final static long serialVersionUID = 1234567890L;
    @XmlElement(required = true)
    protected String systemName;
    @XmlElement(required = true)
    protected String userName;
    protected long userRoleId;
    protected Boolean isUserRoleSystemWide;
    @XmlElement(required = true)
    protected UserContext.UserRights userRights;
    protected long jobExecutionId;
    protected String hostIp;
    protected String hostName;

    /**
     * Gets the value of the systemName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSystemName() {
        return systemName;
    }

    /**
     * Sets the value of the systemName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSystemName(String value) {
        this.systemName = value;
    }

    /**
     * Gets the value of the userName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getUserName() {
        return userName;
    }

    /**
     * Sets the value of the userName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setUserName(String value) {
        this.userName = value;
    }

    /**
     * Gets the value of the userRoleId property.
     * 
     */
    public long getUserRoleId() {
        return userRoleId;
    }

    /**
     * Sets the value of the userRoleId property.
     * 
     */
    public void setUserRoleId(long value) {
        this.userRoleId = value;
    }

    /**
     * Gets the value of the isUserRoleSystemWide property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isIsUserRoleSystemWide() {
        return isUserRoleSystemWide;
    }

    /**
     * Sets the value of the isUserRoleSystemWide property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setIsUserRoleSystemWide(Boolean value) {
        this.isUserRoleSystemWide = value;
    }

    /**
     * Gets the value of the userRights property.
     * 
     * @return
     *     possible object is
     *     {@link UserContext.UserRights }
     *     
     */
    public UserContext.UserRights getUserRights() {
        return userRights;
    }

    /**
     * Sets the value of the userRights property.
     * 
     * @param value
     *     allowed object is
     *     {@link UserContext.UserRights }
     *     
     */
    public void setUserRights(UserContext.UserRights value) {
        this.userRights = value;
    }

    /**
     * Gets the value of the jobExecutionId property.
     * 
     */
    public long getJobExecutionId() {
        return jobExecutionId;
    }

    /**
     * Sets the value of the jobExecutionId property.
     * 
     */
    public void setJobExecutionId(long value) {
        this.jobExecutionId = value;
    }

    /**
     * Gets the value of the hostIp property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHostIp() {
        return hostIp;
    }

    /**
     * Sets the value of the hostIp property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHostIp(String value) {
        this.hostIp = value;
    }

    /**
     * Gets the value of the hostName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHostName() {
        return hostName;
    }

    /**
     * Sets the value of the hostName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHostName(String value) {
        this.hostName = value;
    }


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
     *         &lt;element name="userRightId" type="{http://emias.mos.ru/system/v1/userContext/}ID" maxOccurs="unbounded" minOccurs="0"/&gt;
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
        "userRightIds"
    })
    public static class UserRights
        implements Serializable
    {

        private final static long serialVersionUID = 1234567890L;
        @XmlElement(name = "userRightId", type = Long.class)
        protected List<Long> userRightIds;

        /**
         * Gets the value of the userRightIds property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the userRightIds property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getUserRightIds().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link Long }
         * 
         * 
         */
        public List<Long> getUserRightIds() {
            if (userRightIds == null) {
                userRightIds = new ArrayList<Long>();
            }
            return this.userRightIds;
        }

    }

}
