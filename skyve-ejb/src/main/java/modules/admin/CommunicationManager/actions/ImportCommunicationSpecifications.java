package modules.admin.CommunicationManager.actions;

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
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.BeanValidator;
import org.skyve.util.FileUtil;
import org.skyve.util.JSON;
import org.skyve.web.WebContext;

import modules.admin.CommunicationManager.CommunicationManagerExtension;
import modules.admin.domain.Communication;
import modules.admin.domain.CommunicationManager.ImportActionType;

public class ImportCommunicationSpecifications extends UploadAction<CommunicationManagerExtension> {

	/**
	 * Upload a zip containing a number of Communication configurations
	 * unzip to a temporary folder
	 * and then unmarshall the json
	 */
	@Override
	public CommunicationManagerExtension upload(CommunicationManagerExtension bean, Upload upload, UploadException exception,
			WebContext webContext) throws Exception {

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
	
					if (bean.getImportActionType() == ImportActionType.validateOnlyCommunicationConfigurationsAndTemplates) {
						validateCommunication(pb, false, null);
						webContext.growl(MessageSeverity.info,
								"Communication validated ok - select import option to import this Communication");
						return bean;
					}
					validateCommunication(pb, true, null);
	
					// now load
					pb = (PersistentBean) JSON.unmarshall(CORE.getUser(), json);
	
					loadCommunication(bean, pb);
	
				} catch (Exception e) {
					e.printStackTrace();
					if(e instanceof ValidationException) {
						throw e;
					}
					throw new ValidationException(new Message("The file was not recognised as a valid Communication"));
				}
			} else if (MimeType.zip.getStandardFileSuffix().equals(ext)) {
	
				// store the uploaded zip
				File outdir = CommunicationManagerExtension.getTemporaryPreparationFolder();
				bean.setPathToZip(outdir.getAbsolutePath());
				File importFile = CommunicationManagerExtension.getZipFile();
				Files.copy(in, Paths.get(importFile.getAbsolutePath()));
	
				// extract the Communication configurations from the zip
				FileUtil.extractZipArchive(importFile, outdir);
				
				// handle case that zip supplied was a directory of files
				if (outdir.isDirectory()) {
					if(outdir.listFiles().length==1 && outdir.listFiles()[0].isDirectory()) {
						outdir = outdir.listFiles()[0];
					}
				}
	
				List<String> templatesToReplace = new ArrayList<>();
	
				// Validate all Communications before attempting to replace them
				if (outdir.isDirectory()) {
					for (File communication : outdir.listFiles()) {
						byte[] fileContent = Files.readAllBytes(communication.toPath());
						String json = new String(fileContent, Charset.forName("UTF-8"));
	
						PersistentBean pb;
						try {
							pb = (PersistentBean) JSON.unmarshall(CORE.getUser(), json);
						} catch (Exception e) {
							e.printStackTrace();
							throw new ValidationException(
									new Message("The Communication " + communication.getName() + " was not a valid."));
						}
						validateCommunication(pb, false, templatesToReplace);
					}
	
					if (bean.getImportActionType() == ImportActionType.validateOnlyCommunicationConfigurationsAndTemplates) {
						webContext.growl(MessageSeverity.info,
								"All Communications validated ok - select import option to import Communications");
						return bean;
					}
	
					// if all validated, then proceed by deleting any existing Communication with the same name
					for (String name : templatesToReplace) {
						removePreviousTemplate(name);
					}
	
					// then reload
					for (File communication : outdir.listFiles()) {
						byte[] fileContent = Files.readAllBytes(communication.toPath());
						String json = new String(fileContent, Charset.forName("UTF-8"));
	
						// now load
						PersistentBean pb = (PersistentBean) JSON.unmarshall(CORE.getUser(), json);
						loadCommunication(bean, pb);
					}
				}
			} else {
				throw new ValidationException("File type " + upload.getMimeType().toString() + " is not recognised as either json or zip");
			}
		}
		
		webContext.growl(MessageSeverity.info, "Communications uploaded successfully");

		return bean;
	}

	/**
	 * Validate that the bean is a valid Communication, and optionally remove the previous version (if one exists)
	 * 
	 * @param pb
	 * @param withRemove
	 */
	private static void validateCommunication(final PersistentBean pb, final boolean withRemove,
			final List<String> templatesToReplace) {
		if (pb instanceof Communication) {
			Communication newTemplate = (Communication) pb;
			BeanValidator.validateBeanAgainstDocument(newTemplate);
			BeanValidator.validateBeanAgainstBizlet(newTemplate);

			if (withRemove) {
				removePreviousTemplate(newTemplate.getDescription());
			} else {
				if(templatesToReplace!=null) {
					templatesToReplace.add(newTemplate.getDescription());
				}
			}
		}
	}

	/**
	 * Remove a previous version of the Communication if one exists
	 * 
	 * @param name
	 */
	private static void removePreviousTemplate(String name) {
		Persistence pers = CORE.getPersistence();
		DocumentQuery q = pers.newDocumentQuery(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
		q.getFilter().addEquals(Communication.descriptionPropertyName, name);
		Communication c = q.beanResult();
		if (c != null) {
			pers.delete(c);
			pers.evictCached(c);
		}
	}

	/**
	 * Create and save the new Communication
	 * 
	 * @param bean
	 * @param pb
	 */
	private static void loadCommunication(CommunicationManagerExtension bean, PersistentBean pb) {
		if (pb instanceof Communication) {
			Communication newCommunication = (Communication) pb;

			try {
				newCommunication = CORE.getPersistence().save(newCommunication);
			} catch (Exception e) {
				e.printStackTrace();
				CommunicationManagerExtension.cleanUpTemporaryFiles();
				throw new ValidationException(
						new Message("The Communication template " + newCommunication.getDescription() + " could not be saved."));
			}
		}
	}
}
