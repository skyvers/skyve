package org.skyve.wildcat.web;

import java.awt.Color;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.Principal;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRReport;
import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperFillManager;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.JasperReport;
import net.sf.jasperreports.engine.design.JRDesignBand;
import net.sf.jasperreports.engine.design.JRDesignExpression;
import net.sf.jasperreports.engine.design.JRDesignField;
import net.sf.jasperreports.engine.design.JRDesignLine;
import net.sf.jasperreports.engine.design.JRDesignParameter;
import net.sf.jasperreports.engine.design.JRDesignSection;
import net.sf.jasperreports.engine.design.JRDesignStaticText;
import net.sf.jasperreports.engine.design.JRDesignTextField;
import net.sf.jasperreports.engine.design.JRValidationException;
import net.sf.jasperreports.engine.design.JasperDesign;
import net.sf.jasperreports.engine.type.EvaluationTimeEnum;
import net.sf.jasperreports.engine.type.HorizontalAlignEnum;
import net.sf.jasperreports.engine.type.ModeEnum;
import net.sf.jasperreports.engine.type.PositionTypeEnum;
import net.sf.jasperreports.engine.type.StretchTypeEnum;
import net.sf.jasperreports.j2ee.servlets.BaseHttpServlet;

import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.SessionEndedException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.Query;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.report.ReportFormat;
import org.skyve.wildcat.jasperreports.WildcatDataSource;
import org.skyve.wildcat.jasperreports.ReportDesignParameters;
import org.skyve.wildcat.jasperreports.ReportDesignParameters.ColumnAlignment;
import org.skyve.wildcat.jasperreports.ReportDesignParameters.ReportColumn;
import org.skyve.wildcat.jasperreports.ReportDesignParameters.ReportStyle;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.JSONUtil;
import org.skyve.wildcat.util.ReportUtil;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.service.smartclient.CompoundFilterOperator;
import org.skyve.wildcat.web.service.smartclient.FilterOperator;
import org.skyve.wildcat.web.service.smartclient.SmartClientListServlet;

public class ReportServlet extends HttpServlet {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 1L;

	public static final String REPORT_PATH = "report.rpt";
	public static final String EXPORT_PATH_FRAGMENT = "/export.";
	public static final String REPORT_FORMAT = "_format";

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
					throw new SessionEndedException();
				}
				persistence.setUser(user);
		
				String path = request.getServletPath();
//				if (path.equals('/' + ThinClientWebContext.PARAM_PATH)) {
//					doParams(request, response);
//				}
//				else if (path.equals(ThinClientWebContext.SETUP_PATH)) {
//					doSetup(request, response);
//				}
				if (path.equals('/' + REPORT_PATH)) {
					doReport(request, response);
				}
				else if (path.contains(EXPORT_PATH_FRAGMENT)) {
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
/*
	private void doSetup(HttpServletRequest request, HttpServletResponse response) 
	throws IOException {
		PrintWriter pw = response.getWriter();
		try {
			pw.print("<html><head/><body><form id=\"dummy\" name=\"dummy\" action=\"export.");
			pw.print(request.getParameter("format")); 
			pw.print("\" method=\"post\"><input type=\"hidden\" name=\"values\" value=\"");
			pw.print(request.getParameter("values").replace("\"", "&quot;"));
			pw.print("\"/></form><script type=\"text/javascript\">document.forms[0].submit()</script></body></html>");
		}
		catch (Exception e) {
			System.err.println("Problem getting the report parameters - " + e.toString());
			e.printStackTrace();
		}
		finally {
			pw.close();
		}
	}
*/
/*
	private static void doParams(HttpServletRequest request, HttpServletResponse response)
	throws IOException {
		ServletOutputStream out = response.getOutputStream();
		try {
			String documentName = request.getParameter(WebContext.DOCUMENT_NAME);
			if (documentName == null) {
				throw new ServletException("No document name in the URL");
			}
			int dotIndex = documentName.indexOf('.');
			String moduleName = documentName.substring(0, dotIndex);
			documentName = documentName.substring(dotIndex + 1);

			String reportName = request.getParameter(WebContext.REPORT_NAME);
			if (reportName == null) {
				throw new ServletException("No report name in the URL");
			}
			
			User user = Persistence.get().getUser();

			View paramsView = new View();
			
			paramsView.setType(ViewType.params);
			paramsView.setTitle("Select Format");

			BaseWebContext webContext = new BaseWebContext(UUID.randomUUID().toString(), 
															request,
															response);
			RenderContext<BaseWebContext> renderContext = webContext.getRenderContext();
			Renderer<BaseWebContext> renderer = RendererFactory.getRenderer(renderContext, user);
    		renderer.render(moduleName + '.' + documentName, paramsView, (Bean) null);
		}
		catch (Exception e) {
			System.err.println("Problem getting the report parameters - " + e.toString());
			e.printStackTrace();
		}
		finally {
			out.close();
		}
	}
*/
	private static void doReport(HttpServletRequest request, HttpServletResponse response) {
		try (OutputStream out = response.getOutputStream()) {
			String documentName = request.getParameter(AbstractWebContext.DOCUMENT_NAME);
			if (documentName == null) {
				throw new ServletException("No document name in the URL");
			}
			int dotIndex = documentName.indexOf('.');
			String moduleName = documentName.substring(0, dotIndex);
			documentName = documentName.substring(dotIndex + 1);

			String reportName = request.getParameter(AbstractWebContext.REPORT_NAME);
			if (reportName == null) {
				throw new ServletException("No report name in the URL");
			}
			
			User user = AbstractPersistence.get().getUser();
			Customer customer = user.getCustomer();
			Module module = customer.getModule(moduleName);
			Document document = module.getDocument(customer, documentName);
			
			// Find the context bean
			// Note - if there is no form in the view then there is no web context
			Bean bean = WebUtil.getConversationBeanFromRequest(request, response);

	        ReportFormat format = ReportFormat.valueOf(request.getParameter(REPORT_FORMAT));
	        ByteArrayOutputStream baos = new ByteArrayOutputStream();
			JasperPrint jasperPrint = ReportUtil.runReport(user, 
															document, 
															reportName, 
															getParameters(request), 
															bean, 
															format,
															baos);
			pumpOutReportFormat(baos.toByteArray(), jasperPrint, format, request.getSession(), response);
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
			if ((! paramName.equals(AbstractWebContext.DOCUMENT_NAME)) &&
				(! paramName.equals(AbstractWebContext.REPORT_NAME))) {
				params.put(paramName, paramValue);
			}
    	}

    	return params;
	}
	
	private static void pumpOutReportFormat(byte[] bytes, 
												JasperPrint jasperPrint,
												ReportFormat format,
												HttpSession session,
												HttpServletResponse response)
	throws IOException {
		response.setCharacterEncoding(ServletConstants.UTF8);

		switch (format) {
		case txt:
			response.setContentType(MimeType.plain.toString());
			break;
		case csv:
			response.setContentType(MimeType.csv.toString());
			response.setHeader("Content-Disposition", "attachment; filename=\"export.csv\"");
			break;
		case html:
			response.setContentType(MimeType.html.toString());
			response.setHeader("Content-Disposition", "inline; filename=\"export.html\"");
// TODO maybe I should UUEncode this thing to the client
			session.setAttribute(BaseHttpServlet.DEFAULT_JASPER_PRINT_SESSION_ATTRIBUTE, jasperPrint);
			break;
		case xhtml:
			response.setContentType(MimeType.html.toString());
			response.setHeader("Content-Disposition", "inline; filename=\"export.html\"");
// TODO maybe I should UUEncode this thing to the client
			session.setAttribute(BaseHttpServlet.DEFAULT_JASPER_PRINT_SESSION_ATTRIBUTE, jasperPrint);
			break;
		case pdf:
			response.setContentType(MimeType.pdf.toString());
			response.setHeader("Content-Disposition", "attachment; filename=\"export.pdf\"");
			break;
		case xls:
			response.setContentType(MimeType.excel.toString());
			response.setHeader("Content-Disposition", "attachment; filename=\"export.xls\"");
			break;
		case rtf:
			response.setContentType(MimeType.rtf.toString());
			response.setHeader("Content-Disposition", "attachment; filename=\"export.rtf\"");
			break;
		case odt:
			response.setContentType(MimeType.openDocumentText.toString());
			response.setHeader("Content-Disposition", "attachment; filename=\"export.odt\"");
			break;
		case ods:
			response.setContentType(MimeType.openDocumentSpreadsheet.toString());
			response.setHeader("Content-Disposition", "attachment; filename=\"export.ods\"");
			break;
		case docx:
			response.setContentType(MimeType.docx.toString());
			response.setHeader("Content-Disposition", "attachment; filename=\"export.docx\"");
			break;
		case xlsx:
			response.setContentType(MimeType.xlsx.toString());
			response.setHeader("Content-Disposition", "attachment; filename=\"export.xlsx\"");
			break;
		case pptx:
			response.setContentType(MimeType.pptx.toString());
			response.setHeader("Content-Disposition", "attachment; filename=\"export.pptx\"");
			break;
		case xml:
			response.setContentType(MimeType.xml.toString());
			response.setHeader("Content-Disposition", "attachment; filename=\"export.xml\"");
			break;
		default:
			throw new IllegalStateException("Report format " + format + " not catered for.");
		}

		response.setContentLength(bytes.length);
		
		// NEED TO KEEP THIS FOR IE TO SHOW PDFs ACTIVE-X temp files required
		response.setHeader("Cache-Control", "cache");
        response.setHeader("Pragma", "cache");
        response.addDateHeader("Expires", System.currentTimeMillis() + (60000)); // 1 minute
        
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
				AbstractRepository repository = AbstractRepository.get();
	
				String valuesParam = request.getParameter("values");
				if (UtilImpl.HTTP_TRACE) UtilImpl.LOGGER.info(valuesParam);
				if (valuesParam == null) {
					response.setContentType(MimeType.html.toString());
					response.setCharacterEncoding(ServletConstants.UTF8);
					out.write("<html><head><title>Missing Report Parameters</head><body><h1>There are no report parameters in this request</h1></body></html>".getBytes(ServletConstants.UTF8));
					return;
				}
	
				@SuppressWarnings("unchecked")
				Map<String, Object> values = (Map<String, Object>) JSONUtil.unmarshall(user, valuesParam);
				String module_Query = (String) values.get("query");
				int _Index = module_Query.indexOf('_');
				Module module = customer.getModule(module_Query.substring(0, _Index));
				String queryName = module_Query.substring(_Index + 1);
				Query query = module.getQuery(queryName);
				if (query == null) {
					query = module.getDocumentDefaultQuery(customer, queryName);
				}
				Document document = module.getDocument(customer, query.getDocumentName());
				String tagId = (String) values.get("tagId");
				DocumentQuery documentQuery = query.constructDocumentQuery(null, tagId);
				
				// add filter criteria
				@SuppressWarnings("unchecked")
				Map<String, Object> criteria = (Map<String, Object>) values.get("criteria");
				if (criteria != null) {
					String operator = (String) criteria.get("operator");
					if (operator != null) { // advanced criteria
						@SuppressWarnings("unchecked")
						List<Map<String, Object>> advancedCriteria = (List<Map<String, Object>>) criteria.get("criteria");
						SmartClientListServlet.addAdvancedFilterCriteriaToQuery(module,
																					document,
																					user,
																					documentQuery,
																					CompoundFilterOperator.valueOf(operator),
																					advancedCriteria,
																					tagId);
					}
					else { // simple criteria
						SmartClientListServlet.addSimpleFilterCriteriaToQuery(module,
																				document,
																				user,
																				documentQuery,
																				FilterOperator.substring,
																				criteria,
																				tagId);
					}
				}
				JRDataSource dataSource = new WildcatDataSource(user, 
																persistence.iterate(documentQuery).iterator());
	
				ReportDesignParameters designParams = new ReportDesignParameters();
				designParams.setReportFormat(ReportFormat.valueOf((String) values.get("reportFormat")));
				designParams.setReportStyle(ReportStyle.valueOf((String) values.get("style")));
	
				designParams.setPageWidth(((Number) values.get("width")).intValue());
				designParams.setPageHeight(((Number) values.get("height")).intValue());
				designParams.setPaginated(Boolean.TRUE.equals(values.get("isPaginated")));
				designParams.setPretty(Boolean.TRUE.equals(values.get("isPretty")));
	
				designParams.setTopMargin(((Number) values.get("top")).intValue());
				designParams.setLeftMargin(((Number) values.get("bottom")).intValue());
				designParams.setBottomMargin(((Number) values.get("left")).intValue());
				designParams.setRightMargin(((Number) values.get("right")).intValue());
	
				@SuppressWarnings("unchecked")
				List<Map<String, Object>> columns = (List<Map<String, Object>>) values.get("columns");
				for (Map<String, Object> column : columns) {
					ReportColumn reportColumn = new ReportColumn();
					reportColumn.setLine(((Number) column.get("line")).intValue());
					reportColumn.setName((String) column.get("name"));
					reportColumn.setTitle((String) column.get("title"));
					reportColumn.setType("text");
					reportColumn.setWidth(((Number) column.get("width")).intValue());
					reportColumn.setAlignment(ColumnAlignment.valueOf((String) column.get("align")));
					designParams.getColumns().add(reportColumn);
				}
				
				JasperDesign jasperDesign = createJasperDesign(designParams);
				JasperReport jasperReport = JasperCompileManager.compileReport(jasperDesign);
	
				Map<String, Object> params = new TreeMap<>();
//				params.put("RESOURCE_DIR", 
//								repository.REPOSITORY_DIRECTORY + repository.CUSTOMERS_NAMESPACE + 
//								customer.getName() + '/' + repository.RESOURCES_NAMESPACE);
				StringBuilder sb = new StringBuilder(256);
				sb.append(UtilImpl.getAbsoluteBasePath()).append(repository.CUSTOMERS_NAMESPACE);
				sb.append(customer.getName()).append('/').append(repository.RESOURCES_NAMESPACE);
				params.put("RESOURCE_DIR", sb.toString());
				params.put("TITLE", query.getDisplayName());
				
				JasperPrint jasperPrint = JasperFillManager.fillReport(jasperReport,
																		params, 
																		dataSource);
	
				ByteArrayOutputStream baos = new ByteArrayOutputStream();
				ReportFormat format = ReportFormat.valueOf((String) values.get("reportFormat"));
				ReportUtil.runReport(jasperPrint, format, baos);
				pumpOutReportFormat(baos.toByteArray(),
										jasperPrint, 
										format,
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

	private static JasperDesign createJasperDesign(ReportDesignParameters params)
	throws JRException {
		int reportColumnWidth = params.getPageWidth() - params.getLeftMargin() - params.getRightMargin();
		
		// JasperDesign
		JasperDesign jasperDesign = new JasperDesign();
		jasperDesign.setName("NoXmlDesignReport");
		jasperDesign.setLanguage(JRReport.LANGUAGE_GROOVY);
		jasperDesign.setPageWidth(params.getPageWidth());
		jasperDesign.setPageHeight(params.getPageHeight());
		jasperDesign.setColumnWidth(reportColumnWidth);
		jasperDesign.setColumnSpacing(0);
		jasperDesign.setLeftMargin(params.getLeftMargin());
		jasperDesign.setRightMargin(params.getRightMargin());
		jasperDesign.setTopMargin(params.getTopMargin());
		jasperDesign.setBottomMargin(params.getBottomMargin());
		jasperDesign.setIgnorePagination(! params.isPaginated());
		
		// Parameters
		JRDesignParameter parameter = new JRDesignParameter();
		parameter.setName("TITLE");
		parameter.setValueClass(String.class);
		jasperDesign.addParameter(parameter);

		parameter = new JRDesignParameter();
		parameter.setName("RESOURCE_DIR");
		parameter.setValueClass(java.lang.String.class);
		jasperDesign.addParameter(parameter);

		// TODO allow grouping here
		
		JRDesignBand band;
		JRDesignStaticText staticText;
		JRDesignTextField textField;
		JRDesignLine line;
		JRDesignExpression expression;

		JRDesignBand columnHeaderBand = new JRDesignBand();
		if (ReportStyle.tabular.equals(params.getReportStyle())) {
			columnHeaderBand.setHeight(18);
		}

		JRDesignBand detailBand = new JRDesignBand();
		if (ReportStyle.tabular.equals(params.getReportStyle())) {
			detailBand.setHeight(20);
		}
		else {
			detailBand.setHeight(params.getColumns().size() * 20 + 10);
		}
		
		int xPos = 0;
		int yPos = 0;
		int columnarLabelWidth = 0;
		
		// Determine the size of the labels (for columnar reports)
		if (ReportStyle.columnar.equals(params.getReportStyle())) {
			for (ReportColumn column : params.getColumns()) {
				columnarLabelWidth = Math.max(columnarLabelWidth, column.getTitle().length() * 8);
			}
		}
		
		for (ReportColumn column : params.getColumns()) {
			// Field
			JRDesignField designField = new JRDesignField();
			designField.setName(column.getName());
			designField.setDescription(column.getName());
			designField.setValueClass(String.class);
			jasperDesign.addField(designField);
			
			HorizontalAlignEnum alignment = null;
			switch (column.getAlignment()) {
			case left:
				alignment = HorizontalAlignEnum.LEFT;
				break;
			case center:
				alignment = HorizontalAlignEnum.CENTER;
				break;
			case right:
				alignment = HorizontalAlignEnum.RIGHT;
				break;
			default:
			}

			// Detail
			if (ReportStyle.tabular.equals(params.getReportStyle())) {
				// Column Header
				staticText = new JRDesignStaticText();
				staticText.setMode(ModeEnum.OPAQUE);
				staticText.setX(xPos);
				staticText.setY(0);
				staticText.setWidth(column.getWidth());
				staticText.setHeight(18);
				staticText.setHorizontalAlignment(HorizontalAlignEnum.CENTER);
				staticText.setForecolor(Color.white);
				staticText.setBackcolor(new Color(0x99, 0x99, 0x99));
				staticText.setFontSize(12);
				staticText.setText(column.getTitle());
				columnHeaderBand.addElement(staticText);

				// Value
				textField = new JRDesignTextField();
				textField.setBlankWhenNull(true);
				textField.setX(xPos);
				textField.setY(0);
				textField.setWidth(column.getWidth());
				textField.setHeight(20);
				textField.setHorizontalAlignment(alignment);
				textField.setFontSize(12);
				textField.setStretchWithOverflow(true);
				textField.setStretchType(StretchTypeEnum.RELATIVE_TO_TALLEST_OBJECT);
				expression = new JRDesignExpression();
				expression.setValueClass(String.class);
				expression.setText("$F{" + column.getName() + "}");
				textField.setExpression(expression);
				detailBand.addElement(textField);
			}
			else {
				// Label
				staticText = new JRDesignStaticText();
				staticText.setX(0);
				staticText.setY(yPos);
				staticText.setWidth(columnarLabelWidth);
				staticText.setHeight(20);
				staticText.setFontSize(12);
				staticText.setItalic(true);
				staticText.setText(column.getTitle());
				detailBand.addElement(staticText);
				
				// Value
				textField = new JRDesignTextField();
				textField.setBlankWhenNull(true);
				textField.setX(150);
				textField.setY(yPos);
				textField.setWidth(reportColumnWidth - columnarLabelWidth);
				textField.setHeight(20);
				textField.setHorizontalAlignment(HorizontalAlignEnum.LEFT);
				textField.setFontSize(12);
				textField.setStretchWithOverflow(true);
				textField.setStretchType(StretchTypeEnum.RELATIVE_TO_TALLEST_OBJECT);
				expression = new JRDesignExpression();
				expression.setValueClass(String.class);
				expression.setText("$F{" + column.getName() + "}");
				textField.setExpression(expression);
				detailBand.addElement(textField);
			}
			
			if (ReportStyle.tabular.equals(params.getReportStyle())) {
				xPos += column.getWidth() + (params.isPretty() ? 5 : 0);
			}
			else {
				yPos += 20;
			}
		}
		
		// Background
		
		band = new JRDesignBand();
		jasperDesign.setBackground(band);
		
		// Title
		band = new JRDesignBand();
		if (params.isPretty()) {
			band.setHeight(58);
			line = new JRDesignLine();
			line.setX(0);
			line.setY(8);
			line.setWidth(reportColumnWidth);
			line.setHeight(1);
			band.addElement(line);
			textField = new JRDesignTextField();
			textField.setBlankWhenNull(true);
			textField.setX(0);
			textField.setY(13);
			textField.setWidth(reportColumnWidth);
			textField.setHeight(35);
			textField.setHorizontalAlignment(HorizontalAlignEnum.CENTER);
			textField.setFontSize(26);
			textField.setBold(true);
			expression = new JRDesignExpression();
			expression.setValueClass(String.class);
			expression.setText("$P{TITLE}");
			textField.setExpression(expression);
			band.addElement(textField);
			line = new JRDesignLine();
			line.setPositionType(PositionTypeEnum.FIX_RELATIVE_TO_BOTTOM);
			line.setX(0);
			line.setY(51);
			line.setWidth(reportColumnWidth);
			line.setHeight(1);
			band.addElement(line);
		}
		jasperDesign.setTitle(band);

		// Page header
		band = new JRDesignBand();
		jasperDesign.setPageHeader(band);

		// Column header
		jasperDesign.setColumnHeader(columnHeaderBand);
		
		// Detail
// TODO remove		jasperDesign.setDetail(detailBand);
		((JRDesignSection) jasperDesign.getDetailSection()).addBand(detailBand);
		
		// Column footer
		band = new JRDesignBand();
		jasperDesign.setColumnFooter(band);

		// Page footer
		band = new JRDesignBand();

		if (params.isPaginated()) {
			band.setHeight(26);
			
			// Current time
			textField = new JRDesignTextField();
			textField.setEvaluationTime(EvaluationTimeEnum.REPORT);
			textField.setPattern("");
			textField.setBlankWhenNull(false);
			textField.setX(30);
			textField.setY(6);
			textField.setWidth(209);
			textField.setHeight(19);
			textField.setForecolor(Color.black);
			textField.setBackcolor(Color.white);
			textField.setFontSize(10);
			expression = new JRDesignExpression();
			expression.setValueClass(Date.class);
			expression.setText("new Date()");
			textField.setExpression(expression);
			band.addElement(textField);
	
			// Page number of
			textField = new JRDesignTextField();
			textField.setPattern("");
			textField.setBlankWhenNull(false);
			textField.setX(reportColumnWidth - 200);
			textField.setY(6);
			textField.setWidth(155);
			textField.setHeight(19);
			textField.setForecolor(Color.black);
			textField.setBackcolor(Color.white);
			textField.setHorizontalAlignment(HorizontalAlignEnum.RIGHT);
			textField.setFontSize(10);
			expression = new JRDesignExpression();
			expression.setValueClass(String.class);
			expression.setText("\"Page \" + $V{PAGE_NUMBER} + \" of\"");
			textField.setExpression(expression);
			band.addElement(textField);
			
			// Total pages
			textField = new JRDesignTextField();
			textField.setEvaluationTime(EvaluationTimeEnum.REPORT);
			textField.setPattern("");
			textField.setBlankWhenNull(false);
			textField.setX(reportColumnWidth - 40);
			textField.setY(6);
			textField.setWidth(40);
			textField.setHeight(19);
			textField.setForecolor(Color.black);
			textField.setBackcolor(Color.white);
			textField.setFontSize(10);
			expression = new JRDesignExpression();
			expression.setValueClass(String.class);
			expression.setText("$V{PAGE_NUMBER}");
			textField.setExpression(expression);
			band.addElement(textField);
		}
		jasperDesign.setPageFooter(band);

		// Summary
		band = new JRDesignBand();
		jasperDesign.setSummary(band);

		return jasperDesign;
	}
}
