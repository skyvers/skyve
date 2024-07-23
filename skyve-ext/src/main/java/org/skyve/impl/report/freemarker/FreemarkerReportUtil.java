package org.skyve.impl.report.freemarker;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.ParametersAreNonnullByDefault;

import org.apache.commons.beanutils.DynaBean;
import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.app.admin.ReportDataset;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.domain.app.admin.ReportParameter;
import org.skyve.domain.app.admin.ReportParameter.Type;
import org.skyve.domain.app.admin.ReportTemplate;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.converters.Converter;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.persistence.DocumentQuery;
import org.skyve.report.ReportFormat;
import org.skyve.util.Util;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.xhtmlrenderer.pdf.ITextOutputDevice;
import org.xhtmlrenderer.pdf.ITextRenderer;
import org.xhtmlrenderer.pdf.ITextUserAgent;
import org.xhtmlrenderer.resource.XMLResource;
import org.xml.sax.InputSource;

import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.BaseFont;

import freemarker.cache.ClassTemplateLoader;
import freemarker.cache.ConditionalTemplateConfigurationFactory;
import freemarker.cache.FileExtensionMatcher;
import freemarker.cache.MultiTemplateLoader;
import freemarker.cache.OrMatcher;
import freemarker.cache.StringTemplateLoader;
import freemarker.cache.TemplateLoader;
import freemarker.core.HTMLOutputFormat;
import freemarker.core.TemplateConfiguration;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;

public final class FreemarkerReportUtil {
	private static Configuration cfg;
	private static PathMatchingResourcePatternResolver resolver;
	private static StringTemplateLoader strl;

	private FreemarkerReportUtil() {
		// disallow instantiation
	}

	public static void init() {
		// Create your Configuration instance, and specify if up to what FreeMarker
		// version (here 2.3.29) do you want to apply the fixes that are not 100%
		// backward-compatible. See the Configuration JavaDoc for details.
		cfg = new Configuration(Configuration.VERSION_2_3_29);

		// Specify the source where the template files come from
		ClassLoader cl = Thread.currentThread().getContextClassLoader();
		ClassTemplateLoader ctl = new ClassTemplateLoader(cl, "/modules");
		ClassTemplateLoader ctl2 = new ClassTemplateLoader(cl, "/templates");
		SkyveDatastoreTemplateLoader sdtl = new SkyveDatastoreTemplateLoader();
		strl = new StringTemplateLoader();

		// Use a resolver based on the context class loader
		resolver = new PathMatchingResourcePatternResolver(cl);

		// Define a multi-template loader in the order we want templates discovered
		MultiTemplateLoader mtl = new MultiTemplateLoader(new TemplateLoader[] { strl, sdtl, ctl, ctl2 });
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
		cfg.setSharedVariable("content", new ContentDirective());
		cfg.setSharedVariable("description", new DescriptionDirective());
		cfg.setSharedVariable("displayName", new DisplayNameDirective());
		cfg.setSharedVariable("dynamicImage", new DynamicImageDirective());
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

	/**
	 * Adds a new in-memory String based template to the list of templates Freemarker will search
	 * for when attempting to resolve templates to merge.
	 * 
	 * @param templateName The name of the template, e.g. <code>myDynamicReport</code>
	 * @param templateMarkup The markup in the template, e.g. <code>"Hello ${user}"</code>
	 */
	public static void addTemplate(final String templateName, final String templateMarkup) {
		strl.putTemplate(templateName, templateMarkup);
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
	public static String createBeanReport(final Bean bean, final String reportName, final Map<String, Object> reportParameters)
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
	public static File createBeanReportPDF(final Bean bean,
											final String reportName,
											final Map<String, Object> reportParameters,
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
	public static String createReport(final String templateName, final Map<String, Object> reportParameters) throws Exception {
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
	public static File createReportPDF(final String templateName, final Map<String, Object> reportParameters, final String reportFilename)
	throws Exception {
		// retrieve the report template
		Template template = getTemplate(templateName);

		// create the report
		return createReport(template, reportParameters, reportFilename);
	}

	/**
	 * Executes a {@link ReportTemplate} which has been created and saved in the database using the
	 * parameters and datasets defined in the template or supplied here, and prepares it ready to
	 * serve in a {@link DownloadAction}.
	 * 
	 * @param reportName The template name of the report in the database, e.g. myReport.flth
	 * @param reportParameters An optional map of parameters which will replace any existing parameters already
	 *        defined in the report template
	 * @param format The output format of the report, CSV or PDF
	 * @param downloadFilename The filename of the report (without the file extension)
	 * @return A download to be returned from a {@link DownloadAction}
	 * @throws Exception
	 */
	public static Download downloadReport(final String reportName,
											final Map<String, Object> reportParameters,
											final ReportFormat format,
											final String downloadFilename)
	throws Exception {
		final String reportOutput = runReport(reportName, reportParameters);

		// convert merged report output from String to an InputStream
		byte[] content = reportOutput.getBytes(Util.UTF8);

		// if CSV, return the stream
		if (format == ReportFormat.csv) {
			return new Download(String.format("%s.csv", downloadFilename), content, MimeType.csv);
		}

		// convert to PDF (writing to PDF requires an OutputStream)
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		generatePDFFromHTML(new ByteArrayInputStream(content), baos);

		return new Download(String.format("%s.pdf", downloadFilename), baos.toByteArray(), MimeType.pdf);
	}

	/**
	 * Generates a PDF from a HTML file
	 * 
	 * @param in An input stream containing the source HTML
	 * @param outputFile The file to write the resulting PDF file to
	 * @throws Exception
	 */
	public static void generatePDFFromHTML(InputStream in, File outputFile)
	throws Exception {
		try (OutputStream out = new FileOutputStream(outputFile)) {
			generatePDFFromHTML(in, out);
		}
	}

	/**
	 * Generates a PDF from a HTML file
	 *
	 * @param in An input stream containing the source HTML
	 * @param outputStream The outputStream to write the resulting PDF to
	 * @throws Exception
	 */
	public static void generatePDFFromHTML(InputStream in, OutputStream outputStream)
	throws Exception {
		ITextRenderer renderer = new ITextRenderer();
		ResourceLoaderUserAgent callback = new ResourceLoaderUserAgent(renderer.getOutputDevice(),
				renderer.getSharedContext().getDotsPerPixel());
		renderer.getSharedContext().setUserAgentCallback(callback);

		loadFonts(renderer);

		org.w3c.dom.Document doc = XMLResource.load(in).getDocument();

		renderer.createPDF(doc, outputStream);
	}

	/**
	 * Generates a PDF from a HTML file
	 * 
	 * @param url The path to the HTML file on the filesystem
	 * @param outputFile The file to write the resulting PDF file to
	 * @throws Exception
	 */
	public static void generatePDFFromHTML(String url, File outputFile)
	throws Exception {
		try (OutputStream os = new FileOutputStream(outputFile)) {
			ITextRenderer renderer = new ITextRenderer();
			ResourceLoaderUserAgent callback = new ResourceLoaderUserAgent(renderer.getOutputDevice(),
					renderer.getSharedContext().getDotsPerPixel());
			renderer.getSharedContext().setUserAgentCallback(callback);

			loadFonts(renderer);

			org.w3c.dom.Document doc = XMLResource.load(new InputSource(url)).getDocument();

			renderer.createPDF(doc, os);
		}
	}

	public static Template getBeanReport(final Bean bean, final String reportName)
	throws Exception {
		final String templateName = String.format("%s/%s/reports/%s", bean.getBizModule(), bean.getBizDocument(), reportName);
		return cfg.getTemplate(templateName);
	}

	public static Template getTemplate(final String templateName)
	throws Exception {
		return CORE.getPersistence().withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
			try {
				return cfg.getTemplate(templateName);
			}
			catch (IOException e) {
				throw new DomainException(e);
			}
		});
	}

	/**
	 * Removes the template with the specified name if it was added earlier using {@link #addTemplate(String, String)}.
	 * 
	 * <p>
	 * Note that this method is not thread safe! Don't call it after FreeMarker has started
	 * using this template loader.
	 * </p>
	 * 
	 * @param templateName Exactly the key with which the template was added.
	 * @return Whether a template was found with the given key (and hence was removed now)
	 */
	public static boolean removeTemplate(final String templateName) {
		return strl.removeTemplate(templateName);
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
	public static String runReport(final String reportName, final Map<String, Object> reportParameters)
	throws Exception {
		// get the report template with the specified name
		ReportTemplate reportTemplate = retrieveReportTemplate(reportName);

		Map<String, Object> root = new HashMap<>();

		// if any parameters were passed in, overwrite any with the same name in the template
		List<? extends ReportParameter> parameters = reportTemplate.getParameters();
		if (reportParameters != null) {
			for (ReportParameter param : parameters) {
				if (reportParameters.containsKey(param.getName())) {
					if (Type.date == param.getType()) {
						Converter<DateOnly> dateConverter = CORE.getCustomer().getDefaultDateConverter();
						DateOnly date = (DateOnly) reportParameters.get(param.getName());
						param.setReportInputValue(dateConverter.toDisplayValue(date));
					} else {
						param.setReportInputValue((String) reportParameters.get(param.getName()));
					}
				}
			}
		}

		// put the parameters into the root
		root.put("reportParameters", parameters);

		// put all the datasets into the root
		for (ReportDataset dataset : reportTemplate.getDatasets()) {
			DatasetType type = dataset.getDatasetType();
			if (type != null) {
				switch (type) {
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
					default:
						throw new IllegalStateException(type + " is not catered for");
				}
			}
		}

		Template template = getTemplate(reportName);

		try (StringWriter sw = new StringWriter()) {
			return CORE.getPersistence().withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
				try {
					template.process(root, sw);
				}
				catch (TemplateException | IOException e) {
					throw new DomainException(e);
				}
				return sw.toString();
			});
		}
	}

	private static File createReport(final Template template, final Map<String, Object> reportParameters, final String reportFilename)
	throws Exception {
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

	/**
	 * Searches the fonts directory on the classpath for any true type fonts.
	 * 
	 * @return List of *.ttf font resources found in the fonts directory
	 * @throws IOException
	 */
	private static List<Resource> getFontResources() throws IOException {
		return Arrays.asList(resolver.getResources("classpath:fonts/*.ttf"));
	}

	/**
	 * Searches the fonts/unicode directory on the classpath for any unicode true type fonts.
	 * 
	 * @return List of *.ttf font resources found in the fonts directory
	 * @throws IOException
	 */
	private static List<Resource> getUnicodeFontResources() throws IOException {
		return Arrays.asList(resolver.getResources("classpath:fonts/unicode/*.ttf"));
	}

	/**
	 * Attempts to automatically load any fonts found in the fonts directory found on the classpath.
	 * 
	 * @param renderer The PDF Renderer to embed the font into
	 * @throws IOException
	 */
	private static void loadFonts(ITextRenderer renderer) throws IOException {
		// load any fonts found on the classpath in fonts/
		try {
			getFontResources().stream()
					.forEach(r -> {
						try {
							File f = r.getFile();
							renderer.getFontResolver().addFont(f.toString(), true);
							Util.LOGGER.info("Loaded font for PDF: " + r.getFilename());
						}
						catch (DocumentException | IOException e) {
							Util.LOGGER.warning("Error loading font file: " + r.getFilename());
							e.printStackTrace();
						}
					});

			// load any unicode fonts found on the classpath in fonts/unicode/
			getUnicodeFontResources().stream()
					.forEach(r -> {
						try {
							File f = r.getFile();
							// required to load unicode fonts
							renderer.getFontResolver().addFont(f.toString(), BaseFont.IDENTITY_H, true);
							Util.LOGGER.info("Loaded unicode font for PDF: " + r.getFilename());
						}
						catch (DocumentException | IOException e) {
							Util.LOGGER.warning("Error loading unicode font file: " + r.getFilename());
							e.printStackTrace();
						}
					});
		}
		catch (FileNotFoundException fnfe) {
			// fonts directory not defined or empty
			Util.LOGGER.warning("Error loading fonts for report: " + fnfe.getMessage());
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
	private static ReportTemplate retrieveReportTemplate(final String templateName) {
		return CORE.getPersistence().withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
			DocumentQuery q = p.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME, AppConstants.REPORT_TEMPLATE_DOCUMENT_NAME);
			q.getFilter().addEquals(AppConstants.TEMPLATE_NAME_ATTRIBUTE_NAME, templateName);
			ReportTemplate template = q.beanResult();
			if (template == null) {
				throw new DomainException(String.format("No report template with the name '%s' could be found", templateName));
			}

			return template;
		});
	}

	@ParametersAreNonnullByDefault
	private static class ResourceLoaderUserAgent extends ITextUserAgent {
		private ResourceLoaderUserAgent(ITextOutputDevice outputDevice, int dotsPerPixel) {
			super(outputDevice, dotsPerPixel);
		}

		@Override
		protected InputStream resolveAndOpenStream(String uri) {
			InputStream is = super.resolveAndOpenStream(uri);
			return is;
		}
	}
}
