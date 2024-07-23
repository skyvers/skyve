package modules.admin.ReportManager.actions;

import java.io.File;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.admin.ReportDataset.DatasetType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.BeanValidator;
import org.skyve.util.FileUtil;
import org.skyve.util.JSON;
import org.skyve.web.WebContext;

import modules.admin.ReportManager.ReportManagerExtension;
import modules.admin.ReportTemplate.ReportTemplateExtension;
import modules.admin.domain.ReportManager.ImportActionType;
import modules.admin.domain.ReportTemplate;

public class ImportReportSpecifications extends UploadAction<ReportManagerExtension> {

	/**
	 * Upload a zip containing a number of report configurations
	 * unzip to a temporary folder
	 * and then unmarshall the json
	 */
	@Override
	public ReportManagerExtension upload(ReportManagerExtension bean, Upload upload, UploadException exception, WebContext webContext) throws Exception {

		// Mimetype not being detected, so use file extension instead
		String ext = upload.getFileName().substring(upload.getFileName().lastIndexOf(".") + 1);

		// check mimetype of uploaded file
		try (InputStream in = upload.getInputStream()) {
			if (MimeType.json.getStandardFileSuffix().equals(ext)) {
				byte[] fileContent = new byte[in.available()];
				in.read(fileContent);
				String json = new String(fileContent, Charset.forName("UTF-8"));
	
				try {
					PersistentBean pb = (PersistentBean) JSON.unmarshall(CORE.getUser(), json);
	
					if (bean.getImportActionType() == ImportActionType.validateOnlyReportConfigurationsAndTemplates) {
						validateReport(pb, false, null);
						webContext.growl(MessageSeverity.info, "Report validated ok - select import option to import this report");
						return bean;
					}
					validateReport(pb, true, null);
	
					// now load
					pb = (PersistentBean) JSON.unmarshall(CORE.getUser(), json);
	
					loadReport(bean, pb);
	
				} catch (Exception e) {
					e.printStackTrace();
					if(e instanceof ValidationException) {
						throw e;
					}
					throw new ValidationException(new Message("The file was not recognised as a valid report"));
				}
			} else if (MimeType.zip.getStandardFileSuffix().equals(ext)) {
	
				// store the uploaded zip
				File outdir = ReportManagerExtension.getTemporaryPreparationFolder();
				bean.setPathToZip(outdir.getAbsolutePath());
				File importFile = ReportManagerExtension.getZipFile();
				Files.copy(in, Paths.get(importFile.getAbsolutePath()));
	
				// extract the report configurations from the zip
				FileUtil.extractZipArchive(importFile, outdir);
				
				// handle case that zip supplied was a directory of files
				if (outdir.isDirectory()) {
					if(outdir.listFiles().length==1 && outdir.listFiles()[0].isDirectory()) {
						outdir = outdir.listFiles()[0];
					}
				}
	
				List<String> templatesToReplace = new ArrayList<>();
	
				// Validate all reports before attempting to replace them
				if (outdir.isDirectory()) {
					for (File report : outdir.listFiles()) {
						byte[] fileContent = Files.readAllBytes(report.toPath());
						String json = new String(fileContent, Charset.forName("UTF-8"));
	
						PersistentBean pb;
						try {
							pb = (PersistentBean) JSON.unmarshall(CORE.getUser(), json);
						} catch (Exception e) {
							e.printStackTrace();
							throw new ValidationException(new Message("The report " + report.getName() + " was not a valid."));
						}
						validateReport(pb, false, templatesToReplace);
					}
	
					if (bean.getImportActionType() == ImportActionType.validateOnlyReportConfigurationsAndTemplates) {
						webContext.growl(MessageSeverity.info, "All reports validated ok - select import option to import reports");
						return bean;
					}
	
					// if all validated, then proceed by deleting any existing report with the same name
					for (String name : templatesToReplace) {
						removePreviousTemplate(name);
					}
	
					// then reload
					for (File report : outdir.listFiles()) {
						byte[] fileContent = Files.readAllBytes(report.toPath());
						String json = new String(fileContent, Charset.forName("UTF-8"));
	
						// now load
						PersistentBean pb = (PersistentBean) JSON.unmarshall(CORE.getUser(), json);
						loadReport(bean, pb);
					}
				}
			} else {
				throw new ValidationException("File type " + upload.getMimeType().toString() + " is not recognised as either json or zip");
			}
		}
		
		webContext.growl(MessageSeverity.info, "Reports uploaded successfully");

		return bean;
	}

	/**
	 * Validate that the bean is a valid report, and optionally remove the previous version (if one exists)
	 * 
	 * @param pb
	 * @param withRemove
	 */
	private static void validateReport(final PersistentBean pb, final boolean withRemove, final List<String> templatesToReplace) {
		if (pb instanceof ReportTemplate) {
			ReportTemplate newTemplate = (ReportTemplate) pb;
			BeanValidator.validateBeanAgainstDocument(newTemplate);
			BeanValidator.validateBeanAgainstBizlet(newTemplate);
			
			// validate no SQL dataset if this is a multi-tenant installation
			if (UtilImpl.CUSTOMER == null) {
				if (newTemplate.getDatasets().stream().anyMatch(d -> d.getDatasetType() == DatasetType.SQL)) {
					throw new ValidationException("SQL dataset in report template " + newTemplate.getName()
							+ " is not supported in multi-tenant applications");
				}
			}
					
			if (withRemove) {
				removePreviousTemplate(newTemplate.getName());
			} else {
				if(templatesToReplace!=null) {
					templatesToReplace.add(newTemplate.getName());
				}
			}
		}
	}

	/**
	 * Remove a previous version of the report if one exists
	 * 
	 * @param name
	 */
	private static void removePreviousTemplate(String name) {
		Persistence pers = CORE.getPersistence();
		DocumentQuery q = pers.newDocumentQuery(ReportTemplate.MODULE_NAME, ReportTemplate.DOCUMENT_NAME);
		q.getFilter().addEquals(ReportTemplate.namePropertyName, name);
		ReportTemplate rt = q.beanResult();
		if (rt != null) {
			pers.delete(rt);
			pers.evictCached(rt);
		}
	}

	/**
	 * Create and save the new report
	 * 
	 * @param bean
	 * @param pb
	 */
	private static void loadReport(ReportManagerExtension bean, PersistentBean pb) {
		if (pb instanceof ReportTemplate) {
			ReportTemplateExtension newTemplate = (ReportTemplateExtension) pb;

			// clear the schedule component before saving
			newTemplate.clearSchedules();

			try {
				newTemplate = CORE.getPersistence().save(newTemplate);
			} catch (Exception e) {
				e.printStackTrace();
				ReportManagerExtension.cleanUpTemporaryFiles();
				throw new ValidationException(new Message("The report template " + newTemplate.getName() + " could not be saved."));
			}
		}
	}
}
