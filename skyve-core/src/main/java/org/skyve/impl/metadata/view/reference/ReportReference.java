package org.skyve.impl.metadata.view.reference;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.view.Parameterizable;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.report.ReportFormat;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * This reference can open a report (with parameters).
 * 
 * @author mike
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class ReportReference implements Reference, Parameterizable {
	private static final long serialVersionUID = 558858254713666224L;

	private String moduleName;
	private String documentName;
	private String reportName;
	private ReportFormat format;
	private List<Parameter> parameters = new ArrayList<>();
	
	public String getModuleName() {
		return moduleName;
	}

	@XmlAttribute(required = true)
	public void setModuleName(String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}

	public String getDocumentName() {
		return documentName;
	}

	@XmlAttribute(required = true)
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	public String getReportName() {
		return reportName;
	}

	@XmlAttribute(required = true)
	public void setReportName(String reportName) {
		this.reportName = UtilImpl.processStringValue(reportName);
	}

	public ReportFormat getFormat() {
		return format;
	}

	@XmlAttribute
	public void setFormat(ReportFormat format) {
		this.format = format;
	}

	@Override
	@XmlElementWrapper(namespace = XMLMetaData.VIEW_NAMESPACE, name = "parameters")
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE,
					name = "parameter",
					type = ParameterImpl.class,
					required = false)
	public List<Parameter> getParameters() {
		return parameters;
	}
}
