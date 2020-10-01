package services.report;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.inject.Singleton;

import org.apache.commons.beanutils.DynaBean;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.Util;
import org.xhtmlrenderer.pdf.ITextOutputDevice;
import org.xhtmlrenderer.pdf.ITextRenderer;
import org.xhtmlrenderer.pdf.ITextUserAgent;
import org.xhtmlrenderer.resource.XMLResource;
import org.xml.sax.InputSource;

import com.lowagie.text.DocumentException;

import freemarker.cache.ClassTemplateLoader;
import freemarker.cache.ConditionalTemplateConfigurationFactory;
import freemarker.cache.FileExtensionMatcher;
import freemarker.cache.MultiTemplateLoader;
import freemarker.cache.OrMatcher;
import freemarker.cache.TemplateLoader;
import freemarker.core.HTMLOutputFormat;
import freemarker.core.ParseException;
import freemarker.core.TemplateConfiguration;
import freemarker.template.Configuration;
import freemarker.template.MalformedTemplateNameException;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateNotFoundException;
import modules.admin.ReportDataset.ReportDatasetExtension;
import modules.admin.ReportParameter.ReportParameterExtension;
import modules.admin.domain.ReportTemplate;
import services.report.freemarker.DescriptionDirective;
import services.report.freemarker.DisplayNameDirective;
import services.report.freemarker.FormatDirective;
import services.report.freemarker.ImageDirective;
import services.report.freemarker.ResourceDirective;
import services.report.freemarker.SkyveDatastoreTemplateLoader;
import services.report.freemarker.SqlFormatDirective;

@Singleton
public class ReportService {

	private Configuration cfg;

	private Configuration getInstance() throws IOException, TemplateModelException {
		if (cfg == null) {
			// Create your Configuration instance, and specify if up to what FreeMarker
			// version (here 2.3.29) do you want to apply the fixes that are not 100%
			// backward-compatible. See the Configuration JavaDoc for details.
			cfg = new Configuration(Configuration.VERSION_2_3_29);

			// Specify the source where the template files come from
			ClassTemplateLoader ctl = new ClassTemplateLoader(getClass(), "/modules");
			ClassTemplateLoader ctl2 = new ClassTemplateLoader(getClass(), "/templates");
			SkyveDatastoreTemplateLoader sdtl = new SkyveDatastoreTemplateLoader();

			// Define a multi-template loader in the order we want templates discovered
			MultiTemplateLoader mtl = new MultiTemplateLoader(new TemplateLoader[] { sdtl, ctl, ctl2 });
			cfg.setTemplateLoader(mtl);

			// From here we will set the settings recommended for new projects. These
			// aren't the defaults for backward compatibilty.

			// Set the preferred charset template files are stored in. UTF-8 is
			// a good choice in most applications:
			cfg.setDefaultEncoding("UTF-8");

			// Sets how errors will appear.
			// During web page *development* TemplateExceptionHandler.HTML_DEBUG_HANDLER is better.
			// cfg.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER);
			cfg.setTemplateExceptionHandler(TemplateExceptionHandler.HTML_DEBUG_HANDLER);

			// Don't log exceptions inside FreeMarker that it will thrown at you anyway:
			cfg.setLogTemplateExceptions(false);

			// Wrap unchecked exceptions thrown during template processing into TemplateException-s:
			cfg.setWrapUncheckedExceptions(true);

			// Do not fall back to higher scopes when reading a null loop variable:
			cfg.setFallbackOnNullLoopVariable(false);

			// associate all templates with the HTML extension to the HTML output format
			TemplateConfiguration tcHTML = new TemplateConfiguration();
			tcHTML.setOutputFormat(HTMLOutputFormat.INSTANCE);

			cfg.setTemplateConfigurations(
					new ConditionalTemplateConfigurationFactory(new OrMatcher(
							new FileExtensionMatcher("html"),
							new FileExtensionMatcher("htm")),
							tcHTML));

			// define shared variables for custom directives
			cfg.setSharedVariable("description", new DescriptionDirective());
			cfg.setSharedVariable("displayName", new DisplayNameDirective());
			cfg.setSharedVariable("format", new FormatDirective());
			cfg.setSharedVariable("image", new ImageDirective());
			cfg.setSharedVariable("resource", new ResourceDirective());
			cfg.setSharedVariable("sqlformat", new SqlFormatDirective());

			// define alias skyve date formats
			// Map<String, TemplateDateFormatFactory> customDateFormats = new HashMap<String, TemplateDateFormatFactory>();

			// TODO read the format string out of the customer selected converter
			// customDateFormats.put("skyveDate", new AliasTemplateDateFormatFactory("dd-MMM-yyyy"));
			// customDateFormats.put("skyveDateTime", new AliasTemplateDateFormatFactory("dd-MMM-yyyy hh:mm"));
			// customDateFormats.put("skyveTimestamp", new AliasTemplateDateFormatFactory("dd-MMM-yyyy HH:mm:ss a"));
			// cfg.setCustomDateFormats(customDateFormats);
		}

		return cfg;
	}

	/**
	 * Creates a new Freemarker report and returns the merged output as a String, ready to be processed further.
	 * 
	 * @param bean The Skyve document where the report is located, e.g. <code>admin.User</code>
	 * @param reportName The name of the report in the report package of the document
	 * @param reportParameters Parameters/datasets to fill the report
	 * @return A String with the merged output of the template with the report parameters
	 * @throws Exception
	 */
	public String createBeanReport(final Bean bean, final String reportName, final Map<String, Object> reportParameters)
			throws Exception {
		// retrieve the report template
		Template template = getBeanReport(bean, reportName);

		// create the report
		if (template != null) {
			try (StringWriter sw = new StringWriter()) {
				template.process(reportParameters, sw);
				return sw.toString();
			}
		}

		return null;
	}

	/**
	 * Creates a new Freemarker PDF report and writes the file to disk ready to be processed further (e.g. DownloadAction, sent via
	 * email).
	 * 
	 * @param bean The Skyve document where the report is located, e.g. <code>admin.User</code>
	 * @param reportName The name of the report in the report package of the document
	 * @param reportParameters Parameters/datasets to fill the report
	 * @param reportFilename The output filename of the report, excluding the file extension
	 * @return A temporary {@link File} pointing to the written out PDF report on disk
	 * @throws Exception
	 */
	public File createBeanReportPDF(final Bean bean, final String reportName, final Map<String, Object> reportParameters,
			final String reportFilename) throws Exception {
		// retrieve the report template
		Template template = getBeanReport(bean, reportName);

		// create the report
		return createReport(template, reportParameters, reportFilename);
	}

	/**
	 * Creates a new Freemarker report and returns the merged output as a String, ready to be processed further.
	 * 
	 * @param templateName The name of the report including the path relative to the project root, e.g.
	 *        <code>moduleName/reports/reportName.html</code>
	 * @param reportParameters Parameters/datasets to fill the report
	 * @return A String with the merged output of the template with the report parameters
	 * @throws Exception
	 */
	public String createReport(final String templateName, final Map<String, Object> reportParameters) throws Exception {
		// retrieve the report template
		Template template = getTemplate(templateName);

		// create the report
		if (template != null) {
			try (StringWriter sw = new StringWriter()) {
				template.process(reportParameters, sw);
				return sw.toString();
			}
		}

		return null;
	}

	/**
	 * Creates a new Freemarker PDF report and writes the file to disk ready to be processed further (e.g. DownloadAction, sent via
	 * email).
	 * 
	 * @param templateName The name of the report including the path relative to the project root, e.g.
	 *        <code>moduleName/reports/reportName.html</code>
	 * @param reportParameters Parameters/datasets to fill the report
	 * @param reportFilename The output filename of the report, excluding the file extension
	 * @return A temporary {@link File} pointing to the written out PDF report on disk
	 * @throws Exception
	 */
	public File createReportPDF(final String templateName, final Map<String, Object> reportParameters, final String reportFilename)
			throws Exception {
		// retrieve the report template
		Template template = getTemplate(templateName);

		// create the report
		return createReport(template, reportParameters, reportFilename);
	}

	/**
	 * Generates a PDF from a HTML file
	 * 
	 * @param in An input stream containing the source HTML
	 * @param outputFile The file to write the resulting PDF file to
	 * @throws FileNotFoundException
	 * @throws IOException
	 * @throws DocumentException
	 */
	public void generatePDFFromHTML(InputStream in, File outputFile) throws FileNotFoundException, IOException, DocumentException {
		generatePDFFromHTML(in, new FileOutputStream(outputFile));
	}

	/**
	 * Generates a PDF from a HTML file
	 *
	 * @param in An input stream containing the source HTML
	 * @param outputStream The outputStream to write the resulting PDF to
	 * @throws FileNotFoundException
	 * @throws IOException
	 * @throws DocumentException
	 */
	public void generatePDFFromHTML(InputStream in, OutputStream outputStream) throws FileNotFoundException, IOException, DocumentException {
		try {
			ITextRenderer renderer = new ITextRenderer();
			ResourceLoaderUserAgent callback = new ResourceLoaderUserAgent(renderer.getOutputDevice());
			callback.setSharedContext(renderer.getSharedContext());
			renderer.getSharedContext().setUserAgentCallback(callback);

			loadFonts(renderer);

			org.w3c.dom.Document doc = XMLResource.load(in).getDocument();

			renderer.setDocument(doc, "/");
			renderer.layout();
			renderer.createPDF(outputStream);

			outputStream.close();
		} finally {
			if (outputStream != null) {
				try {
					outputStream.close();
				} catch (IOException e) {
					// ignore
				}
			}
		}
	}

	/**
	 * Generates a PDF from a HTML file
	 * 
	 * @param url The path to the HTML file on the filesystem
	 * @param outputFile The file to write the resulting PDF file to
	 * @throws FileNotFoundException
	 * @throws IOException
	 * @throws DocumentException
	 */
	public void generatePDFFromHTML(String url, File outputFile) throws FileNotFoundException, IOException, DocumentException {
		OutputStream os = null;
		try {
			os = new FileOutputStream(outputFile);

			/* standard approach
			ITextRenderer renderer = new ITextRenderer();
			renderer.setDocument(url);
			renderer.layout();
			renderer.createPDF(os);
			*/

			ITextRenderer renderer = new ITextRenderer();
			ResourceLoaderUserAgent callback = new ResourceLoaderUserAgent(renderer.getOutputDevice());
			callback.setSharedContext(renderer.getSharedContext());
			renderer.getSharedContext().setUserAgentCallback(callback);

			loadFonts(renderer);

			org.w3c.dom.Document doc = XMLResource.load(new InputSource(url)).getDocument();

			renderer.setDocument(doc, url);
			renderer.layout();
			renderer.createPDF(os);

			os.close();
			os = null;
		} finally {
			if (os != null) {
				try {
					os.close();
				} catch (IOException e) {
					// ignore
				}
			}
		}
	}

	public Template getBeanReport(final Bean bean, final String reportName)
			throws TemplateNotFoundException, MalformedTemplateNameException, ParseException, IOException, TemplateModelException {
		final String templateName = String.format("%s/%s/reports/%s", bean.getBizModule(), bean.getBizDocument(), reportName);
		return getInstance().getTemplate(templateName);
	}

	public Template getTemplate(final String templateName)
			throws TemplateNotFoundException, MalformedTemplateNameException, ParseException, IOException, TemplateModelException {
		CORE.getPersistence().setDocumentPermissionScopes(DocumentPermissionScope.customer);
		Template t = getInstance().getTemplate(templateName);
		CORE.getPersistence().resetDocumentPermissionScopes();
		return t;
	}

	/**
	 * Executes a {@link ReportTemplate} which has been created and saved in the database using the
	 * parameters and datasets defined in the template.
	 * 
	 * @param reportName The template name of the report in the database, e.g. myReport.flth
	 * @param reportParameters An optional map of parameters which will replace any existing parameters already
	 *        defined in the report template
	 * @return A String with the merged output of the template with the report parameters
	 * @throws Exception
	 */
	public String runReport(final String reportName, final Map<String, Object> reportParameters) throws Exception {
		// get the report template with the specified name
		ReportTemplate reportTemplate = retrieveReportTemplate(reportName);

		Map<String, Object> root = new HashMap<>();

		// if any parameters were passed in, overwrite any with the same name in the template
		List<ReportParameterExtension> parameters = reportTemplate.getParameters();
		if (reportParameters != null) {
			for (ReportParameterExtension param : parameters) {
				if (reportParameters.containsKey(param.getName())) {
					param.setReportInputValue((String) reportParameters.get(param.getName()));
				}
			}
		}

		// put the parameters into the root
		root.put("reportParameters", parameters);

		// put all the datasets into the root
		for (ReportDatasetExtension dataset : reportTemplate.getDatasets()) {
			switch (dataset.getDatasetType()) {
				case bizQL:
					List<Bean> results = dataset.executeQuery();
					if (results != null) {
						root.put(dataset.getDatasetName(), results);
					}
					break;
				case SQL:
					List<DynaBean> sqlResults = dataset.executeSQLQuery();
					if (sqlResults != null) {
						root.put(dataset.getDatasetName(), sqlResults);
					}
					break;
				case constant:
					root.put(dataset.getDatasetName(), dataset.getQuery());
					break;
				case classValue:
					List<DynaBean> beanResults = dataset.executeClass();
					if (beanResults != null) {
						root.put(dataset.getDatasetName(), beanResults);
					}
					break;
			}
		}

		Template template = getTemplate(reportName);

		try (StringWriter sw = new StringWriter()) {
			CORE.getPersistence().setDocumentPermissionScopes(DocumentPermissionScope.customer);
			template.process(root, sw);
			CORE.getPersistence().resetDocumentPermissionScopes();
			return sw.toString();
		}
	}

	private File createReport(final Template template, final Map<String, Object> reportParameters, final String reportFilename)
			throws IOException, TemplateException, DocumentException {
		if (template != null) {
			try (StringWriter sw = new StringWriter()) {
				template.process(reportParameters, sw);

				// write the output string to an input stream
				InputStream inputStream = new ByteArrayInputStream(sw.toString().getBytes(Charset.forName("UTF-8")));

				Path tempDir = Paths.get(Util.getContentDirectory(), "temp");
				tempDir.toFile().mkdirs();

				File pdfFile = tempDir.resolve(String.format("%s.pdf", reportFilename)).toFile();
				pdfFile.deleteOnExit();

				generatePDFFromHTML(inputStream, pdfFile);

				return pdfFile;
			}
		}

		return null;
	}

	private static void loadFonts(ITextRenderer renderer) throws IOException {
		Path fontPath = Paths.get(Util.getContentDirectory(), "fonts");
		if (fontPath.toFile().exists()) {
			Files.list(fontPath)
					.map(Path::toFile)
					.filter(f -> f.getName().endsWith(".ttf"))
					.forEach(f -> {
						try {
							renderer.getFontResolver().addFont(f.toString(), true);
							Util.LOGGER.fine("Loaded font for PDF: " + f.toString());
						} catch (DocumentException | IOException e) {
							Util.LOGGER.warning("Error loading font file: " + f.toString());
							e.printStackTrace();
						}
					});
		}
	}

	/**
	 * Retrieves a saved {@link ReportTemplate} with the specified name from the datastore.
	 * 
	 * This is not to be confused with {@link ReportService#getTemplate(String)} which retrieves
	 * the Freemarker markup for a report.
	 * 
	 * @param templateName The name of the report template to retrieve
	 * @return The ReportTemplate, if one is found
	 */
	private ReportTemplate retrieveReportTemplate(final String templateName) {
		CORE.getPersistence().setDocumentPermissionScopes(DocumentPermissionScope.customer);
		DocumentQuery q = CORE.getPersistence().newDocumentQuery(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
		q.getFilter().addEquals(ReportTemplate.templateNamePropertyName, templateName);
		ReportTemplate template = q.beanResult();
		CORE.getPersistence().resetDocumentPermissionScopes();

		if (template == null) {
			throw new DomainException(String.format("No report template with the name '%s' could be found", templateName));
		}

		return template;
	}

	private static class ResourceLoaderUserAgent extends ITextUserAgent {
		public ResourceLoaderUserAgent(ITextOutputDevice outputDevice) {
			super(outputDevice);
		}

		protected InputStream resolveAndOpenStream(String uri) {
			InputStream is = super.resolveAndOpenStream(uri);
			return is;
		}
	}
}
