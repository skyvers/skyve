package org.skyve.impl.web;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.Principal;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.domain.messages.AccessException;
import org.skyve.impl.domain.messages.SecurityException;
import org.skyve.impl.generate.jasperreports.DesignSpecification;
import org.skyve.impl.generate.jasperreports.JasperReportRenderer;
import org.skyve.impl.generate.jasperreports.ReportDesignGenerator;
import org.skyve.impl.generate.jasperreports.ReportDesignGeneratorFactory;
import org.skyve.impl.metadata.model.document.field.ConvertableField;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.report.jasperreports.JasperReportUtil;
import org.skyve.impl.report.jasperreports.ReportDesignParameters;
import org.skyve.impl.report.jasperreports.ReportDesignParameters.ColumnAlignment;
import org.skyve.impl.report.jasperreports.ReportDesignParameters.ReportColumn;
import org.skyve.impl.report.jasperreports.ReportDesignParameters.ReportStyle;
import org.skyve.impl.report.jasperreports.SkyveDataSource;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.service.smartclient.CompoundFilterOperator;
import org.skyve.impl.web.service.smartclient.SmartClientFilterOperator;
import org.skyve.impl.web.service.smartclient.SmartClientListServlet;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.router.UxUi;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.report.ReportFormat;
import org.skyve.util.JSON;
import org.skyve.util.Util;

import jakarta.servlet.ServletException;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JasperFillManager;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.JasperReport;
import net.sf.jasperreports.engine.design.JRValidationException;
import net.sf.jasperreports.j2ee.servlets.BaseHttpServlet;

public class ReportServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;

	public static final String REPORT_PATH = "/report";
	public static final String EXPORT_PATH = "/export";

	@Override
	public void doPost(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		doGet(request, response);
	}

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		AbstractPersistence persistence = AbstractPersistence.get();
		try {
			try {
				persistence.begin();
				Principal userPrincipal = request.getUserPrincipal();
				User user = WebUtil.processUserPrincipalForRequest(request, (userPrincipal == null) ? null : userPrincipal.getName(), true);
				if (user == null) {
					throw new SessionEndedException(request.getLocale());
				}
				persistence.setUser(user);

				String path = request.getServletPath();
				if (REPORT_PATH.equals(path)) {
					doReport(request, response);
				}
				else if (EXPORT_PATH.equals(path)) {
					doExport(request, response);
				}
				else {
					throw new IllegalStateException("Report path " + path + " is unknown.");
				}
			}
			catch (Exception e) {
				persistence.rollback();
				throw new ServletException("Could not setup the user in ReportServlet", e);
			}
		}
		finally {
			if (persistence != null) {
				persistence.commit(true);
			}
		}
	}

	private static void doReport(HttpServletRequest request, HttpServletResponse response) {
		try (OutputStream out = response.getOutputStream()) {
			String moduleName = request.getParameter(AbstractWebContext.MODULE_NAME);
			if (moduleName == null) {
				throw new ServletException("No module name in the URL");
			}
			String documentName = request.getParameter(AbstractWebContext.DOCUMENT_NAME);
			if (documentName == null) {
				throw new ServletException("No document name in the URL");
			}
			// If report name is not specified, generate the report.
			String reportName = request.getParameter(AbstractWebContext.REPORT_NAME);
			boolean generatedReport = false;
			if (reportName == null) {
				generatedReport = true;
			}
			String formatString = request.getParameter(AbstractWebContext.REPORT_FORMAT);
			if (formatString == null) {
				throw new ServletException("No report format in the URL");
			}
			ReportFormat format = ReportFormat.valueOf(formatString);

			User user = AbstractPersistence.get().getUser();
			Customer customer = user.getCustomer();
			Module module = customer.getModule(moduleName);
			Document document = module.getDocument(customer, documentName);

			// Find the context bean
			// Note - if there is no form in the view then there is no web context
			String contextKey = request.getParameter(AbstractWebContext.CONTEXT_NAME);
			AbstractWebContext webContext = StateUtil.getCachedConversation(contextKey, request, response);
			Bean bean = WebUtil.getConversationBeanFromRequest(webContext, request);

			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			final JasperPrint jasperPrint;
			if (generatedReport) {
				reportName = String.format("%s - %s", moduleName, documentName);

				final DesignSpecification designSpecification = new DesignSpecification();
				designSpecification.setName("EditView");
				designSpecification.setModuleName(moduleName);
				designSpecification.setDocumentName(documentName);

				final ReportDesignGenerator generator = ReportDesignGeneratorFactory.getGeneratorForDesign(designSpecification);
				generator.populateDesign(designSpecification);
				designSpecification.setMode(DesignSpecification.Mode.bean);
				designSpecification.setDefinitionSource(DesignSpecification.DefinitionSource.view);
				designSpecification.setReportType(DesignSpecification.ReportType.report);

				final JasperReportRenderer reportRenderer = new JasperReportRenderer(designSpecification);

				final Map<String, Object> parameters = getParameters(request);
				parameters.put(JasperReportRenderer.DESIGN_SPEC_PARAMETER_NAME, designSpecification);
				jasperPrint = JasperReportUtil.runReport(reportRenderer.getReport(), user, document, parameters, bean, format, baos);
			}
			else {
				final String isList = request.getParameter(AbstractWebContext.IS_LIST);
				if (isList != null && Boolean.parseBoolean(isList)) {
					final String queryName = request.getParameter(AbstractWebContext.QUERY_NAME);
					final String modelName = request.getParameter(AbstractWebContext.MODEL_NAME);
					final String documentOrQueryOrModelName = (modelName != null) ? modelName : ((queryName != null) ? queryName : documentName);
					final ListModel<Bean> listModel = JasperReportUtil.getQueryListModel(module, documentOrQueryOrModelName);
					jasperPrint = JasperReportUtil.runReport(user, document, reportName, getParameters(request), listModel, format, baos);
				}
				else {
					// Manually load the bean if an id is specified but there is no appropriate bean to load from the conversation.
					final String id = request.getParameter(AbstractWebContext.ID_NAME);
					if ((id != null) && ((bean == null) || ((contextKey != null) && (! contextKey.endsWith(id))))) {
						bean = AbstractPersistence.get().retrieve(document, id);
					}

					jasperPrint = JasperReportUtil.runReport(user, document, reportName, getParameters(request), bean, format, baos);
				}
			}

			pumpOutReportFormat(baos.toByteArray(), jasperPrint, format, reportName, request.getSession(), response);
		}
		catch (Exception e) {
			System.err.println("Problem generating the report - " + e.toString());
			e.printStackTrace();
		}
	}

	private static Map<String, Object> getParameters(HttpServletRequest request) {
//		 TODO coercion somehow...
		Map<String, Object> params = new TreeMap<>();

		for (String paramName : request.getParameterMap().keySet()) {
			String paramValue = request.getParameter(paramName);
			if (! (AbstractWebContext.CONTEXT_NAME.equals(paramName) ||
					AbstractWebContext.ID_NAME.equals(paramName) ||
					AbstractWebContext.REPORT_FORMAT.equals(paramName) ||
					AbstractWebContext.MODULE_NAME.equals(paramName) ||
					AbstractWebContext.DOCUMENT_NAME.equals(paramName) ||
					AbstractWebContext.REPORT_NAME.equals(paramName))) {
				params.put(paramName, paramValue);
				if (UtilImpl.HTTP_TRACE) UtilImpl.LOGGER.info("ReportServlet: Report Parameter " + paramName + " = " + paramValue);
			}
		}

		return params;
	}

	private static void pumpOutReportFormat(byte[] bytes,
												JasperPrint jasperPrint,
												ReportFormat format,
												String fileNameNoSuffix,
												HttpSession session,
												HttpServletResponse response)
	throws IOException {
		response.setCharacterEncoding(Util.UTF8);

		StringBuilder sb = new StringBuilder(64);
		switch (format) {
		case txt:
			response.setContentType(MimeType.plain.toString());
			break;
		case csv:
			response.setContentType(MimeType.csv.toString());
			sb.append("attachment; filename=\"").append(fileNameNoSuffix).append(".csv\"");
			response.setHeader("Content-Disposition", sb.toString());
			break;
		case html:
			response.setContentType(MimeType.html.toString());
			sb.append("inline; filename=\"").append(fileNameNoSuffix).append(".html\"");
			response.setHeader("Content-Disposition", sb.toString());
// TODO maybe I should UUEncode this thing to the client
			session.setAttribute(BaseHttpServlet.DEFAULT_JASPER_PRINT_SESSION_ATTRIBUTE, jasperPrint);
			break;
		case pdf:
			response.setContentType(MimeType.pdf.toString());
			sb.append("attachment; filename=\"").append(fileNameNoSuffix).append(".pdf\"");
			response.setHeader("Content-Disposition", sb.toString());
			break;
		case xls:
			response.setContentType(MimeType.excel.toString());
			sb.append("attachment; filename=\"").append(fileNameNoSuffix).append(".xls\"");
			response.setHeader("Content-Disposition", sb.toString());
			break;
		case rtf:
			response.setContentType(MimeType.rtf.toString());
			sb.append("attachment; filename=\"").append(fileNameNoSuffix).append(".rtf\"");
			response.setHeader("Content-Disposition", sb.toString());
			break;
		case odt:
			response.setContentType(MimeType.openDocumentText.toString());
			sb.append("attachment; filename=\"").append(fileNameNoSuffix).append(".odt\"");
			response.setHeader("Content-Disposition", sb.toString());
			break;
		case ods:
			response.setContentType(MimeType.openDocumentSpreadsheet.toString());
			sb.append("attachment; filename=\"").append(fileNameNoSuffix).append(".ods\"");
			response.setHeader("Content-Disposition", sb.toString());
			break;
		case docx:
			response.setContentType(MimeType.docx.toString());
			sb.append("attachment; filename=\"").append(fileNameNoSuffix).append(".docx\"");
			response.setHeader("Content-Disposition", sb.toString());
			break;
		case xlsx:
			response.setContentType(MimeType.xlsx.toString());
			sb.append("attachment; filename=\"").append(fileNameNoSuffix).append(".xlsx\"");
			response.setHeader("Content-Disposition", sb.toString());
			break;
		case pptx:
			response.setContentType(MimeType.pptx.toString());
			sb.append("attachment; filename=\"").append(fileNameNoSuffix).append(".pptx\"");
			response.setHeader("Content-Disposition", sb.toString());
			break;
		case xml:
			response.setContentType(MimeType.xml.toString());
			sb.append("attachment; filename=\"").append(fileNameNoSuffix).append(".xml\"");
			response.setHeader("Content-Disposition", sb.toString());
			break;
		default:
			throw new IllegalStateException("Report format " + format + " not catered for.");
		}

		response.setContentLength(bytes.length);

		// NEED TO KEEP THIS FOR IE TO SHOW PDFs ACTIVE-X temp files required
		response.setHeader("Cache-Control", "cache");
		response.setHeader("Pragma", "cache");
		response.addDateHeader("Expires", System.currentTimeMillis() + (60000)); // 1 minute
		// The following allows partial requests which are useful for large media or
		// downloading files with pause and resume functions.
		response.setHeader("Accept-Ranges", "bytes");

		try (ServletOutputStream outputStream = response.getOutputStream()) {
			outputStream.write(bytes);
			outputStream.flush();
		}
	}

	private static void doExport(HttpServletRequest request, HttpServletResponse response)
	throws IOException {
		try (ServletOutputStream out = response.getOutputStream()) {
			AbstractPersistence persistence = AbstractPersistence.get();
			try {
				User user = persistence.getUser();
				Customer customer = user.getCustomer();

				String valuesParam = request.getParameter("values");
				if (UtilImpl.HTTP_TRACE) UtilImpl.LOGGER.info(valuesParam);
				if (valuesParam == null) {
					response.setContentType(MimeType.html.toString());
					response.setCharacterEncoding(Util.UTF8);
					out.write("<html><head><title>Missing Report Parameters</head><body><h1>There are no report parameters in this request</h1></body></html>".getBytes(Util.UTF8));
					return;
				}

				@SuppressWarnings("unchecked")
				Map<String, Object> values = (Map<String, Object>) JSON.unmarshall(user, valuesParam);
				String module_QueryOrModel = (String) values.get("ds");
				int _Index = module_QueryOrModel.indexOf('_');
				String moduleName = module_QueryOrModel.substring(0, _Index);
				Module module = customer.getModule(moduleName);
				String documentOrQueryOrModelName = module_QueryOrModel.substring(_Index + 1);
				Document drivingDocument = null;
				ListModel<Bean> model = null;
				UxUi uxui = UserAgent.getUxUi(request);
				int __Index = documentOrQueryOrModelName.indexOf("__");
				if (__Index >= 0) {
					String documentName = documentOrQueryOrModelName.substring(0, __Index);
					String modelName = documentOrQueryOrModelName.substring(__Index + 2);

					if (! user.canAccess(UserAccess.modelAggregate(moduleName, documentName, modelName), uxui.getName())) {
						final String userName = user.getName();
						UtilImpl.LOGGER.warning("User " + userName + " cannot access model " + moduleName + '.' + documentName + '.' + modelName);
						UtilImpl.LOGGER.info("If this user already has a document privilege, check if they were navigated to this page/resource programatically or by means other than the menu or views and need to be granted access via an <accesses> stanza in the module or view XML.");
						throw new AccessException("this data", userName);
					}

					Document document = module.getDocument(customer, documentName);
					model = document.getListModel(customer, modelName, true);

					// Set the context bean in the list model
					// Note - if there is no form in the view then there is no web context
					String contextKey = request.getParameter(AbstractWebContext.CONTEXT_NAME);
					AbstractWebContext webContext = StateUtil.getCachedConversation(contextKey, request, response);
					model.setBean(WebUtil.getConversationBeanFromRequest(webContext, request));

					drivingDocument = model.getDrivingDocument();
				}
				else {
					MetaDataQueryDefinition query = module.getMetaDataQuery(documentOrQueryOrModelName);
					if (query == null) {
						if (! user.canAccess(UserAccess.documentAggregate(moduleName, documentOrQueryOrModelName), uxui.getName())) {
							final String userName = user.getName();
							UtilImpl.LOGGER.warning("User " + userName + " cannot access document " + moduleName + '.' + documentOrQueryOrModelName);
							UtilImpl.LOGGER.info("If this user already has a document privilege, check if they were navigated to this page/resource programatically or by means other than the menu or views and need to be granted access via an <accesses> stanza in the module or view XML.");
							throw new AccessException("this data", userName);
						}
						query = module.getDocumentDefaultQuery(customer, documentOrQueryOrModelName);
					}
					else {
						if (! user.canAccess(UserAccess.queryAggregate(moduleName, documentOrQueryOrModelName), uxui.getName())) {
							final String userName = user.getName();
							UtilImpl.LOGGER.warning("User " + userName + " cannot access query " + moduleName + '.' + documentOrQueryOrModelName);
							UtilImpl.LOGGER.info("If this user already has a document privilege, check if they were navigated to this page/resource programatically or by means other than the menu or views and need to be granted access via an <accesses> stanza in the module or view XML.");
							throw new AccessException("this data", userName);
						}
					}
					if (query == null) {
						throw new ServletException("DataSource does not reference a valid query " + documentOrQueryOrModelName);
					}
					drivingDocument = module.getDocument(customer, query.getDocumentName());
					model = EXT.newListModel(query);
				}

				if (! user.canReadDocument(drivingDocument)) {
					throw new SecurityException("read this data", user.getName());
				}

				String tagId = (String) values.get("tagId");
				model.setSelectedTagId(tagId);

				// add filter criteria
				@SuppressWarnings("unchecked")
				Map<String, Object> criteria = (Map<String, Object>) values.get("criteria");
				if (criteria != null) {
					String operator = (String) criteria.get("operator");
					if (operator != null) { // advanced criteria
						@SuppressWarnings("unchecked")
						List<Map<String, Object>> advancedCriteria = (List<Map<String, Object>>) criteria.get("criteria");
						SmartClientListServlet.addAdvancedFilterCriteriaToQuery(module,
																					drivingDocument,
																					user,
																					CompoundFilterOperator.valueOf(operator),
																					advancedCriteria,
																					tagId,
																					model);
					}
					else { // simple criteria
						SmartClientListServlet.addSimpleFilterCriteriaToQuery(module,
																				drivingDocument,
																				customer,
																				SmartClientFilterOperator.substring,
																				criteria,
																				tagId,
																				model);
					}
				}

				JasperPrint jasperPrint = null;

				try (AutoClosingIterable<Bean> iterable = model.iterate()) {
					JRDataSource dataSource = new SkyveDataSource(user, iterable.iterator());

					ReportDesignParameters designParams = new ReportDesignParameters();
					designParams.setReportFormat(ReportFormat.valueOf((String) values.get("reportFormat")));
					designParams.setReportStyle(ReportStyle.valueOf((String) values.get("style")));

					designParams.setPageWidth(((Number) values.get("width")).intValue());
					designParams.setPageHeight(((Number) values.get("height")).intValue());
					designParams.setPaginated(Boolean.TRUE.equals(values.get("isPaginated")));
					designParams.setPretty(Boolean.TRUE.equals(values.get("isPretty")));
					designParams.setShowSummary(Boolean.TRUE.equals(values.get("showSummary")));

					designParams.setTopMargin(((Number) values.get("top")).intValue());
					designParams.setLeftMargin(((Number) values.get("bottom")).intValue());
					designParams.setBottomMargin(((Number) values.get("left")).intValue());
					designParams.setRightMargin(((Number) values.get("right")).intValue());

					@SuppressWarnings("unchecked")
					List<Map<String, Object>> columns = (List<Map<String, Object>>) values.get("columns");
					for (Map<String, Object> column : columns) {
						ReportColumn reportColumn = new ReportColumn();
						String name = (String) column.get("name");
						// Check for column projection injection - ie the column name supplied
						// 1. has no name, 2. is not part of the given model, is not a projected column in the model
						if (name == null) {
							throw new SecurityException("Malformed report columns", user.getName());
						}
						String binding = name.replace('_', '.');
						if (! PersistentBean.FLAG_COMMENT_NAME.equals(binding)) { // allow bizFlagComment
							MetaDataQueryColumn mdqc = model.getColumns().stream()
																.filter(c -> binding.equals(c.getBinding()) || binding.equals(c.getName()))
																.findAny().orElse(null);
							if (mdqc == null) {
								throw new SecurityException("Non-existent data", user.getName());
							}
							if ((mdqc instanceof MetaDataQueryProjectedColumn) &&
									(! ((MetaDataQueryProjectedColumn) mdqc).isProjected())) {
								throw new SecurityException("Non-projected data", user.getName());
							}
						}
						reportColumn.setName(binding);
						reportColumn.setLine(((Number) column.get("line")).intValue());
						reportColumn.setTitle((String) column.get("title"));
						reportColumn.setWidth(((Number) column.get("width")).intValue());
						String align = (String) column.get("align");
						if (align != null) {
							reportColumn.setAlignment(ColumnAlignment.valueOf(align));
						}
						else {
							reportColumn.setAlignment(ColumnAlignment.left);
						}
						if ((drivingDocument != null) && (reportColumn.getName() != null)) {
							final Attribute attribute = drivingDocument.getPolymorphicAttribute(customer, reportColumn.getName());
							if (attribute != null) {
								reportColumn.setAttributeType(attribute.getAttributeType());
								if (attribute instanceof ConvertableField) {
									Converter<?> converter = ((ConvertableField) attribute).getConverterForCustomer(customer);
									if (converter != null) {
										reportColumn.setFormatPattern(converter.getFormatPattern());
									}
								}
							}
						}

						designParams.getColumns().add(reportColumn);
					}

					final JasperReportRenderer reportRenderer = new JasperReportRenderer(designParams);
					JasperReport jasperReport = reportRenderer.getReport();

					Map<String, Object> params = new TreeMap<>();
					StringBuilder sb = new StringBuilder(256);
					sb.append(UtilImpl.getAbsoluteBasePath()).append(ProvidedRepository.CUSTOMERS_NAMESPACE);
					sb.append(customer.getName()).append('/').append(ProvidedRepository.RESOURCES_NAMESPACE);
					params.put("RESOURCE_DIR", sb.toString());
					params.put("TITLE", model.getLocalisedDescription());

					jasperPrint = JasperFillManager.fillReport(jasperReport, params, dataSource);
				}

				ByteArrayOutputStream baos = new ByteArrayOutputStream();
				ReportFormat format = ReportFormat.valueOf((String) values.get("reportFormat"));
				JasperReportUtil.runReport(jasperPrint, format, baos);

				pumpOutReportFormat(baos.toByteArray(),
									jasperPrint,
									format,
									(String) values.get("fileNameNoSuffix"),
									request.getSession(),
									response);
			}
			catch (Exception e) {
				System.err.println("Problem generating the report - " + e.toString());
				e.printStackTrace();
				out.print("<html><head/><body><h3>");
				if (e instanceof JRValidationException) {
					out.print(e.getLocalizedMessage());
				}
				else {
					out.print("An error occured whilst processing your report.");
				}
				out.print("</body></html>");
			}
			finally {
				persistence.commit(true);
			}
		}
	}
}
