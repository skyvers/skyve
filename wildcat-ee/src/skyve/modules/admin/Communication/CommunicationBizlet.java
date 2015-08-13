package modules.admin.Communication;

import java.io.File;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.List;

import modules.admin.Communication.actions.GetResults;
import modules.admin.domain.Communication;
import modules.admin.domain.Communication.FormatType;
import modules.admin.domain.Tag;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Job;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.wildcat.content.AttachmentContent;
import org.skyve.wildcat.content.ContentManager;

public class CommunicationBizlet extends Bizlet<Communication> {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7404508611264793559L;

	@Override
	public Communication newInstance(Communication bean) throws Exception {

		// set defaults
		bean.setFormatType(FormatType.email);

		return super.newInstance(bean);
	}

	public static void checkForUnsavedData(Communication communication) throws Exception {
		if (!communication.originalValues().isEmpty()) {
			// find if any field except results
			for (String s : communication.originalValues().keySet()) {
				if (!Communication.resultsPropertyName.equals(s)) {
					throw new ValidationException(new Message("You have unsaved changes. The Job cannot be run until data is saved." + s));
				}
			}
		}
	}

	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, Communication bean) throws Exception {

		Persistence pers = CORE.getPersistence();
		List<DomainValue> result = new ArrayList<>();

		Customer customer = pers.getUser().getCustomer();

		if (Communication.documentNamePropertyName.equals(attributeName)) {
			if (bean.getModuleName() != null) {
				Module module = customer.getModule(bean.getModuleName());
				for (String documentName : module.getDocumentRefs().keySet()) {
					Document document = module.getDocument(customer, documentName);
					result.add(new DomainValue(document.getName(), document.getDescription()));
				}
			}
		}

		return result;
	}

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {

		List<DomainValue> result = new ArrayList<>();
		Persistence pers = CORE.getPersistence();

		Customer customer = pers.getUser().getCustomer();
		if (Communication.moduleNamePropertyName.equals(attributeName)) {
			for (Module module : customer.getModules()) {
				result.add(new DomainValue(module.getName(), module.getTitle()));
			}
		}

		if (Communication.tagPropertyName.equals(attributeName)) {

			// look for OTHER tags
			DocumentQuery q = pers.newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
			q.addOrdering(Tag.namePropertyName);

			List<Tag> tags = q.beanResults();
			for (Tag t : tags) {
				result.add(new DomainValue(t.getBizId(), t.getName()));
			}
		}

		return result;
	}

	public static Communication kickOffJob(Communication communication) throws Exception {

		GetResults.getResults(communication);

		Persistence persistence = CORE.getPersistence();
		User user = persistence.getUser();
		Customer customer = user.getCustomer();
		Module module = customer.getModule(Communication.MODULE_NAME);
		Job job = module.getJob("jProcessCommunicationsForTag");

		EXT.runOneShotJob(job, communication, user);

		StringBuilder sb = new StringBuilder();
		sb.append(communication.getResults());
		sb.append("\nThe job has been commenced - check Admin->Jobs for the log.");
		communication.setResults(sb.toString());

		communication.originalValues().remove(Communication.resultsPropertyName);

		return communication;
	}

	public static void send(Communication communication, Bean... beans) throws Exception {

		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();

		String sendTo = Binder.formatMessage(customer, communication.getSendTo(), beans);
		String[] sendOverride = new String[] { sendTo };

		sendOverrideTo(communication, sendOverride, beans);
	}

	public static void generate(Communication communication, Bean... beans) throws Exception {

		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();

		validatePath(communication);

		
		try (FileOutputStream fos = new FileOutputStream(communication.getFilePath())){

			String sendTo = Binder.formatMessage(customer, communication.getSendTo(), beans);
			String emailSubject = Binder.formatMessage(customer, communication.getSubject(), beans);
			String emailBody = communication.getBody();
			emailBody = Binder.formatMessage(customer, emailBody, beans);


			// Generate file name - Communication_Description_To
			StringBuilder fileName = new StringBuilder();
			fileName.append(Communication.descriptionPropertyName);
			fileName.append('_');
			fileName.append(communication.getDescription());
			fileName.append('_');
			fileName.append(sendTo).append(".eml");
			
			// add attachment
			if (communication.getAttachment() != null) {
				
				try (ContentManager cm = EXT.newContentManager()) {
					AttachmentContent content = cm.get(communication.getAttachment());
					byte[] fileBytes = content.getContentBytes();
					String attachmentName = (communication.getAttachmentFileName() == null ? "attachment" : communication.getAttachmentFileName());
					EXT.writeMail(new String[] { sendTo }, null, communication.getSendFrom(), emailSubject, htmlEnclose(emailBody), MimeType.html, attachmentName, fileBytes, content.getMimeType(), fos);
				}				
			} else {
				EXT.writeMail(new String[] { sendTo }, null, communication.getSendFrom(), emailSubject, htmlEnclose(emailBody), MimeType.html, null, null, null, fos);
			}

			fos.flush();
		}

	}

	public static void sendOverrideTo(Communication communication, String[] sendTo, Bean... beans) throws Exception {

		Persistence pers = CORE.getPersistence();
		User user = pers.getUser();
		Customer customer = user.getCustomer();

		String emailSubject = Binder.formatMessage(customer, communication.getSubject(), beans);
		String emailBody = communication.getBody();
		emailBody = Binder.formatMessage(customer, emailBody, beans);

		EXT.sendMail(sendTo, null, communication.getSendFrom(), emailSubject, emailBody, MimeType.html, null, null, null);
	}

	public static void validatePath(Communication communication) throws Exception {
		if (communication == null) {
			throw new Exception("There communication item is null. ");
		}

		String path = communication.getFilePath();
		if (path == null) {
			throw new ValidationException(new Message(Communication.filePathPropertyName, "The directory is empty - you must specify where reports can be created."));
		}
		File reportDir = new File(path);
		if (!reportDir.exists()) {
			reportDir.mkdirs();
			if (!reportDir.exists()) {
				throw new ValidationException(new Message(Communication.filePathPropertyName, "The directory " + reportDir.getAbsolutePath() + " does not exist & can not be created."));
			}
		}

		if (!reportDir.isDirectory()) {
			throw new ValidationException(new Message(Communication.filePathPropertyName, "The directory " + reportDir.getAbsolutePath() + " is not a directory."));
		}
	}
	
	public static String fileNameSafe(String s) {

		return s.replace(',', '_').replace('&', '_')
				.replace('/', '_').replace('\\', '_').replace(" ", "");
	}

	public static String htmlEnclose(String html) {
		StringBuilder sb = new StringBuilder();
		sb.append("<html><body>").append(html).append("</body></html>");
		return sb.toString();
	}

	
}
