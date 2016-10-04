package org.skyve.impl.metadata.repository.view.actions;

import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.report.ReportFormat;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "report")
public class ReportAction extends ParameterizableAction {
	public ReportAction() {
		implicitName = ImplicitActionName.Report;
	}
	
	private String moduleName;
	private String documentName;
	private String reportName;
	private ReportFormat reportFormat;
	
	public String getModuleName() {
		return moduleName;
	}

	@XmlAttribute(required = true)
	public void setModuleName(String moduleName) {
		this.moduleName = moduleName;
	}

	public String getDocumentName() {
		return documentName;
	}

	@XmlAttribute(required = true)
	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}

	public String getReportName() {
		return reportName;
	}

	@XmlAttribute(required = true)
	public void setReportName(String reportName) {
		this.reportName = reportName;
	}

	public ReportFormat getReportFormat() {
		return reportFormat;
	}
	
	@XmlAttribute
	public void setReportFormat(ReportFormat reportFormat) {
		this.reportFormat = reportFormat;
	}
	
	@Override
	public ActionImpl toMetaDataAction() {
		ActionImpl result = super.toMetaDataAction();
		result.setResourceName(reportName);
		if (ImplicitActionName.Report.toString().equals(result.getName())) {
			result.setName(reportName);
		}

		List<Parameter> parameters = result.getParameters();
		ParameterImpl p = new ParameterImpl();
		p.setName(AbstractWebContext.REPORT_NAME);
		p.setValue(reportName);
		parameters.add(p);
		p = new ParameterImpl();
		p.setName(AbstractWebContext.MODULE_NAME);
		p.setValue(moduleName);
		parameters.add(p);
		p = new ParameterImpl();
		p.setName(AbstractWebContext.DOCUMENT_NAME);
		p.setValue(documentName);
		parameters.add(p);
		if (reportFormat != null) {
			p = new ParameterImpl();
			p.setName(AbstractWebContext.REPORT_FORMAT);
			p.setValue(reportFormat.name());
			parameters.add(p);
		}
		
		return result;
	}
}
