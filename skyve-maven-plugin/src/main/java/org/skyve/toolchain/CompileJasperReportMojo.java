package org.skyve.toolchain;

import java.io.File;
import java.io.FileNotFoundException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.skyve.impl.tools.jasperreports.SkyveDocumentExecuterFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.jasperreports.engine.DefaultJasperReportsContext;
import net.sf.jasperreports.engine.JasperCompileManager;
import net.sf.jasperreports.engine.JasperReportsContext;
import net.sf.jasperreports.engine.query.QueryExecuterFactory;

/**
 * Compiles one or more JasperReports templates from the Skyve module tree.
 *
 * <p>Threading: this mojo mutates JasperReports global context and should be treated as thread-confined.
 */
@Mojo(name = "compileJasperReport")
public class CompileJasperReportMojo extends AbstractSkyveMojo {
	private static final Logger LOGGER = LoggerFactory.getLogger(CompileJasperReportMojo.class);
	public static final String JRXML_EXTENSION = "jrxml";
	public static final String JASPER_EXTENSION = "jasper";

	/**
	 * Prompts for a report name, resolves the matching templates, and compiles them to {@code .jasper} files.
	 *
	 * <p>Side effects: writes compiled report files next to the source templates and configures the JasperReports
	 * query executer factory for Skyve document queries.
	 *
	 * @throws MojoExecutionException if compilation fails
	 */
	@Override
	public void execute() throws MojoExecutionException {
		try {
			configureClasspath();
			final String reportName = prompter.prompt("Please enter the name of the report");
			final List<File> reportsToCompile = getReports(reportName);
			if (reportsToCompile.isEmpty()) {
				throw new FileNotFoundException(String.format("Failed to find a report named %s.", reportName));
			}
			JasperReportsContext jasperReportsContext = DefaultJasperReportsContext.getInstance();
			jasperReportsContext.setProperty(QueryExecuterFactory.QUERY_EXECUTER_FACTORY_PREFIX + "document",
												SkyveDocumentExecuterFactory.class.getCanonicalName());
			for (File report : reportsToCompile) {
				final String compiledReportFilename = report.getAbsolutePath().replace(JRXML_EXTENSION, JASPER_EXTENSION);
				JasperCompileManager.compileReportToFile(report.getAbsolutePath(), compiledReportFilename);
				LOGGER.info("Successfully compiled report {} to {}.", report.getAbsolutePath(), compiledReportFilename);
			}
		}
		catch (Exception e) {
			LOGGER.error("Failed to compile report.", e);
			throw new MojoExecutionException("Failed to compile report.", e);
		}
	}

	/**
	 * Finds the matching JasperReports templates for the supplied report name.
	 *
	 * @param reportName the report name to search for, with or without the {@code .jrxml} suffix
	 * @return the matching report templates
	 * @throws FileNotFoundException if the Skyve modules directory cannot be found
	 */
	private List<File> getReports(String reportName) throws FileNotFoundException {
		final Path modulesDirectory = getModulesDirectory();
		LOGGER.info("Searching for reports in {}.", modulesDirectory);
		final String extension = "." + JRXML_EXTENSION;
		final String reportNameWithExtension = reportName.endsWith(extension) ? reportName : reportName + extension;

		final Collection<File> reports = FileUtils.listFiles(modulesDirectory.toFile(),
																new String[] {JRXML_EXTENSION},
																true);
		return reports.stream().filter(report -> report.getName().equals(reportNameWithExtension)).toList();
	}
}
