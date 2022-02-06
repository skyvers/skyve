package modules.whosin.Staff.actions;

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

public class UploadQualifications extends UploadAction<Staff> {

	@Override
	public Staff upload(Staff bean, Upload upload, UploadException exception, WebContext webContext) throws Exception {

		List<StaffQualification> quals = null;
		if (upload.getFileName().endsWith(MimeType.xlsx.getStandardFileSuffix())) {
			int sheetIndex = 0;
			POISheetLoader pl = new POISheetLoader(LoaderActivityType.CREATE_ALL, upload.getInputStream(), sheetIndex, exception,
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
		} else if (upload.getFileName().endsWith(MimeType.csv.getStandardFileSuffix())) {
			CSVLoader dl = new CSVLoader(LoaderActivityType.CREATE_ALL, upload.getInputStream(), exception,
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
