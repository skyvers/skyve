package org.skyve.impl.metadata.repository.view.actions;

import java.util.List;

import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.widget.bound.Parameter;
import org.skyve.report.ReportFormat;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * JAXB-annotated descriptor for a {@code <report>} action button in a view.
 *
 * <p>A report action renders a named Jasper / Skyve report and streams it to
 * the browser.  Extends {@link ParameterizableAction} so runtime parameters
 * can be forwarded to the report engine.
 *
 * <p>Threading: not thread-safe.  Read-only after JAXB unmarshalling.
 *
 * @see ParameterizableAction
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "report")
public class ReportAction extends ParameterizableAction {
	private static final long serialVersionUID = 6083685683273669677L;

	/**
	 * Creates a report action descriptor with implicit report action semantics.
	 */
	public ReportAction() {
		implicitName = ImplicitActionName.Report;
	}
	
	private String moduleName;
	private String documentName;
	private String reportName;
	private ReportFormat reportFormat;
	private Boolean listReport;
	private String queryName;
	private String modelName;
	
	/**
	 * Returns the module containing the report document.
	 *
	 * @return module name
	 */
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * Sets the module containing the report document.
	 *
	 * @param moduleName module name
	 */
	@XmlAttribute(required = true)
	public void setModuleName(String moduleName) {
		this.moduleName = moduleName;
	}

	/**
	 * Returns the document owning the report definition.
	 *
	 * @return document name
	 */
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * Sets the document owning the report definition.
	 *
	 * @param documentName document name
	 */
	@XmlAttribute(required = true)
	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}

	/**
	 * Returns the report resource name to execute.
	 *
	 * @return report name
	 */
	public String getReportName() {
		return reportName;
	}

	/**
	 * Sets the report resource name to execute.
	 *
	 * @param reportName report name
	 */
	@XmlAttribute(required = true)
	public void setReportName(String reportName) {
		this.reportName = reportName;
	}

	/**
	 * Returns the preferred output format.
	 *
	 * @return report format, or {@code null}
	 */
	public ReportFormat getReportFormat() {
		return reportFormat;
	}
	
	/**
	 * Sets the preferred output format.
	 *
	 * @param reportFormat report format
	 */
	@XmlAttribute
	public void setReportFormat(ReportFormat reportFormat) {
		this.reportFormat = reportFormat;
	}

	/**
	 * Indicates whether the report should run in list-report mode.
	 *
	 * @return {@code true} for list-report mode, {@code false} otherwise, or {@code null}
	 */
	public Boolean isListReport() {
		return listReport;
	}

	/**
	 * Sets whether the report should run in list-report mode.
	 *
	 * @param listReport {@code true} for list-report mode, {@code false} otherwise
	 */
	@XmlAttribute
	public void setListReport(Boolean listReport) {
		this.listReport = listReport;
	}

	/**
	 * Returns the named query used to produce list-report data.
	 *
	 * @return query name, or {@code null}
	 */
	public String getQueryName() {
		return queryName;
	}

	/**
	 * Sets the named query used to produce list-report data.
	 *
	 * @param queryName query name
	 */
	@XmlAttribute
	public void setQueryName(String queryName) {
		this.queryName = queryName;
	}

	/**
	 * Returns the list model name supplying rows for report execution.
	 *
	 * @return model name, or {@code null}
	 */
	public String getModelName() {
		return modelName;
	}

	/**
	 * Sets the list model name supplying rows for report execution.
	 *
	 * @param modelName model name
	 */
	@XmlAttribute
	public void setModelName(String modelName) {
		this.modelName = modelName;
	}

	/**
	 * Converts this descriptor to runtime metadata and injects report parameters.
	 *
	 * <p>Side effects: appends framework report parameters to the action parameter
	 * list, including report/module/document identifiers and optional format/query/model
	 * controls.
	 *
	 * @return runtime action metadata configured for report execution
	 */
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
		if (listReport != null) {
			p = new ParameterImpl();
			p.setName(AbstractWebContext.IS_LIST);
			p.setValue(listReport.toString());
			parameters.add(p);
		}
		if (queryName != null) {
			p = new ParameterImpl();
			p.setName(AbstractWebContext.QUERY_NAME);
			p.setValue(queryName);
			parameters.add(p);
		}
		if (modelName != null) {
			p = new ParameterImpl();
			p.setName(AbstractWebContext.MODEL_NAME);
			p.setValue(modelName);
			parameters.add(p);
		}
		
		return result;
	}
}
