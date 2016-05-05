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

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "report")
public class ReportAction extends ParameterizableAction {
	public ReportAction() {
		implicitName = ImplicitActionName.Report;
	}
	
	private String reportName;
	private String doc;
	
	public String getDoc() {
		return doc;
	}

	@XmlAttribute(required = true)
	public void setDoc(String doc) {
		this.doc = doc;
	}

	public String getReportName() {
		return reportName;
	}

	@XmlAttribute(required = true)
	public void setReportName(String reportName) {
		this.reportName = reportName;
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
		p.setName(AbstractWebContext.DOCUMENT_NAME);
		p.setValue(doc);
		parameters.add(p);

		return result;
	}
}
