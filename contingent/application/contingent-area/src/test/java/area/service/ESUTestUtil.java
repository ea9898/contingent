/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package area.service;

import area.service.methods.RestoreAreaTest;
import java.io.InputStream;
import java.io.StringReader;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import static org.xmlunit.assertj.XmlAssert.assertThat;
import org.xmlunit.builder.Input;

/**
 *
 * @author m.kachalov
 */
public class ESUTestUtil {

    private ESUTestUtil(){}
    
    /**
     * Сравнение проверяемого XML с эталонным XML документом.
     * При проверке в эталонном документе меняется значение тэга operationDate,
     * на то значение которое установлено в аналогичном тэге проверяемого XML.
     * 
     * @param pathToEtalonFile путь к эталонному XML-файлу, доступному в classpath. Например: "esu/restoreArea/test2.xml"
     * @param testXml проверяемый XML-документ
     * @throws Exception 
     */
    public static void assertEqualsESUXML(String pathToEtalonFile, String testXml) throws Exception {
        InputStream etalonXmlStream = getFileStream(pathToEtalonFile);
        assertNotNull(etalonXmlStream, "Не найден эталонный XML по пути: " + pathToEtalonFile);
        
        assertThat(testXml).hasXPath(".//operationDate");
        
        Document etalonDocument = setOperationDate(getEtalonXML(etalonXmlStream), getOperationDate(testXml));
        assertThat(testXml).and(Input.fromDocument(etalonDocument)).areIdentical();
    }
    
    private static InputStream getFileStream(String pathToFile) {
        return ESUTestUtil.class.getClassLoader().getResourceAsStream(pathToFile);
    }
    
    private static Document getEtalonXML(InputStream etalonXmlStream) throws Exception {
        DocumentBuilderFactory builderFactory = DocumentBuilderFactory.newInstance();
        builderFactory.setNamespaceAware(true);
        DocumentBuilder documentBuilder = builderFactory.newDocumentBuilder();
        Document document = documentBuilder.parse(etalonXmlStream);
        return document;
    }
    
    private static String getOperationDate(String xmlContent) throws Exception {
        DocumentBuilderFactory builderFactory = DocumentBuilderFactory.newInstance();
        builderFactory.setNamespaceAware(true);
        DocumentBuilder documentBuilder = builderFactory.newDocumentBuilder();
        InputSource is = new InputSource(new StringReader(xmlContent));
        Document document = documentBuilder.parse(is);
        XPath xPath = XPathFactory.newInstance().newXPath();
        Node operationDateNode = (Node) xPath.compile(".//operationDate").evaluate(document, XPathConstants.NODE);
        return operationDateNode.getTextContent();        
    }
    
    /**
     * Меняет значение тэга operationDate в документе.
     * 
     * @param document
     * @param dateTime
     * @return
     * @throws Exception 
     */
    private static Document setOperationDate(Document document, String dateTime) throws Exception {
        XPath xPath = XPathFactory.newInstance().newXPath();
        Node operationDateNode = (Node) xPath.compile(".//operationDate").evaluate(document, XPathConstants.NODE);
        operationDateNode.setTextContent(dateTime);
        return document;
    }
    
}
