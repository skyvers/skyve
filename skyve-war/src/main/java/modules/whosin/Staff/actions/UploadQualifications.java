package modules.whosin.Staff.actions;

import java.io.InputStream;
import java.util.List;

import org.skyve.content.MimeType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.bizport.AbstractDataFileLoader.LoaderActivityType;
import org.skyve.impl.bizport.CSVLoader;
import org.skyve.impl.bizport.POISheetLoader;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.web.WebContext;

import modules.whosin.domain.Staff;
import modules.whosin.domain.StaffQualification;

/**
 * Imports staff qualifications from uploaded CSV or XLSX files.
 */
public class UploadQualifications extends UploadAction<Staff> {
	/**
	 * Parses the uploaded file and appends imported qualifications to the supplied staff bean.
	 *
	 * @param bean the staff bean receiving imported qualifications
	 * @param upload the uploaded data file
	 * @param exception the upload exception collector
	 * @param webContext the active web context
	 * @return the updated staff bean
	 * @throws Exception if parsing or validation fails
	 */
	@Override
	public Staff upload(Staff bean, Upload upload, UploadException exception, WebContext webContext) throws Exception {
		List<StaffQualification> quals = null;
		if (upload.getFileName().endsWith(MimeType.xlsx.getStandardFileSuffix())) {
			try (InputStream is = upload.getInputStream()) {
				int sheetIndex = 0;
				POISheetLoader pl = new POISheetLoader(LoaderActivityType.CREATE_ALL, is, sheetIndex, exception,
						StaffQualification.MODULE_NAME, 
						StaffQualification.DOCUMENT_NAME);
				
				pl.addFields(StaffQualification.namePropertyName, 
				StaffQualification.typePropertyName,
				StaffQualification.issuingOrganisationPropertyName, 
				StaffQualification.descriptionPropertyName,
				StaffQualification.dateAttainedPropertyName, 
				StaffQualification.dateExpiryPropertyName);
				
				pl.setDebugMode(true);
				pl.setDataIndex(1); // has headers
	
				quals = pl.beanResults();
			}
		} else if (upload.getFileName().endsWith(MimeType.csv.getStandardFileSuffix())) {
			try (InputStream is = upload.getInputStream()) {
				CSVLoader dl = new CSVLoader(LoaderActivityType.CREATE_ALL, is, exception,
						StaffQualification.MODULE_NAME, 
						StaffQualification.DOCUMENT_NAME,
						StaffQualification.namePropertyName, 
						StaffQualification.typePropertyName,
						StaffQualification.issuingOrganisationPropertyName, 
						StaffQualification.descriptionPropertyName,
						StaffQualification.dateAttainedPropertyName, 
						StaffQualification.dateExpiryPropertyName);
				dl.setDebugMode(true);
				dl.setDataIndex(1); // has headers
	
				quals = dl.beanResults();
			}
		} else {
			throw new ValidationException(new Message("Only csv or xlsx file types are supported"));
		}
		for (StaffQualification q : quals) {
			q.setParent(bean);
			bean.getQualifications().add(q);
		}
//		bean = CORE.getPersistence().save(bean);

		return bean;
	}
}
