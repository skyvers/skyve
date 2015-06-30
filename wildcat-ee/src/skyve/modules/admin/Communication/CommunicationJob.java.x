package modules.admin.Communication;

import java.io.File;
import java.io.FileOutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import modules.admin.domain.Communication;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.job.WildcatJob;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;

public final class CommunicationJob extends WildcatJob {

	/**
	 * 
	 */
	private static final long serialVersionUID = 6439083166710967250L;

	// for logging
	private String jobName;

	@Override
	public String cancel() {
		return null;
	}

	/**
	 * Validate/process any preconditions before starting the job.
	 * 
	 * @param search
	 * @param system
	 * @throws Exception
	 */
	public void preProcess(Communication mailout) throws Exception {
		if (mailout.getTag() == null) {
			throw new ValidationException(new Message(Communication.tagPropertyName, "A tag must be selected for the mailout."));
		}
	};

	/**
	 * Manipulate any data to do with each tagged document. A transaction has
	 * begun but you can commit and begin within this method if required.
	 * 
	 * @throws Exception
	 */
	public void process(Communication mailout, Persistence persistence, String basePath, List<String> log) throws Exception {

		try {
//			ContactExt c = (ContactExt) GrowerBizlet.getContactFromSubscriptionType(grower, mailout.getSubscriptionType());
//			ContactBizlet.loadUserDetail(c, grower);

//			if ((c == null) || (c.getEmail1() == null)) {

				// compile validation message
//				StringBuilder vm = new StringBuilder();
//				vm.append("Unable to get Email Address for Grower '");
//				if (grower.getOwner() != null) {
//					vm.append(grower.getOwner().getName());
//				} else {
//					vm.append("G").append(grower.getGrowerNo());
//				}
//				vm.append("' - not able to email report for ").append(mailout.getSubscriptionType().toDescription());

//				throw new ValidationException(new Message(vm.toString()));
//			}


			// Generate file name
//			String padded = "0000" + grower.getGrowerNo();
//			padded = padded.substring(padded.length() - 5);
//			StringBuilder fileName = new StringBuilder(128);
//			fileName.append(padded).append('_');
//			if (grower.getOwner() != null && grower.getOwner().getName() != null) {
//				fileName.append(grower.getOwner().getName());
//			} else if (grower.getOperator() != null && grower.getOperator().getName() != null) {
//				fileName.append(grower.getOperator().getName());
//			} else {
//				fileName.append("Grower");
//			}

//			fileName.append('_').append(mailout.getSubscriptionType().toDescription()).append(".eml");

//			StringBuilder path = new StringBuilder(basePath);
//			path.append(pgibsaUtil.fileNameSafe(fileName.toString()));
//			FileOutputStream fos = new FileOutputStream(path.toString());
//			try {
//				String emailSubject = null;
//				String emailBody = null;
//				User user = persistence.getUser();
//				Customer customer = user.getCustomer();
//				emailSubject = Binder.formatMessage(customer, mailout.getMailoutEmailSubject(), grower, c);
//				emailBody = Binder.formatMessage(customer, mailout.getMailoutEmailBody(), grower, c);
//
//				// add attachment
//				if (mailout.getAttachment1() != null) {
//					Session session = ContentUtil.getDefaultSession();
//					StreamContent stream = ContentUtil.get(session, mailout.getAttachment1());
//					byte[] fileBytes = stream.getBytes();
//					String attachmentName = (mailout.getAttachment1FileName() == null ? "attachment" : mailout.getAttachment1FileName());
////					EXT.writeMail(new String[] { c.getEmail1() }, null, system.getFromEmailAddress(), emailSubject, pgibsaUtil.htmlEnclose(emailBody), MimeType.html, attachmentName, fileBytes, stream.getMimeType(), fos);
//				} else {
////					EXT.writeMail(new String[] { c.getEmail1() }, null, system.getFromEmailAddress(), emailSubject, pgibsaUtil.htmlEnclose(emailBody), MimeType.html, null, null, null, fos);
//				}
//
//				fos.flush();
//			} finally {
//				fos.close();
//			}
//
//			log.add("        Report email file is " + path.toString());
		} catch (Throwable t) {
			log.add("Encountered an error during the job run: " + t.getMessage());
			t.printStackTrace();
		}

	};

	@Override
	public final void execute() throws Exception {
		List<String> log = getLog();
		Persistence persistence = CORE.getPersistence();

		Communication communication = (Communication) getBean();
		preProcess(communication);

		String basePath = communication.getFilePath();
		String separator = java.lang.System.getProperty("file.separator");

		StringBuilder path = new StringBuilder(128);
		path.append(basePath);
		if (!basePath.endsWith(separator)) {
			path.append(separator);
		}
		path.append("bulk_mail_");
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy_MM_dd_HH_mm_ss");
		path.append(sdf.format(new Date()));
		path.append(separator);
		basePath = path.toString();
		path.append(separator);

		new File(path.toString()).mkdirs();
	
		log.add("Started " + jobName + " Job at " + new Date() + " to folder " + path.toString());

		// extract the tag from the search criteria instance
		String tagId = communication.getTag().getBizId();
		log.add("  Processing tag " + communication.getTag().getName());

		int communicationsProcessed = 0;

		persistence.begin();

		// Iterate over the tag and stash all the growers in a list
		// Sort that list by grower number
//		List<Grower> growers = new ArrayList<Grower>();
//		for (Bean bean : EXT.iterateTagged(tagId)) {
//			Grower grower = GrowerUtil.getGrower(bean);
//			if (grower != null) {
//				growers.add(grower);
//			}
//		}
//		Binder.sortCollectionByOrdering(growers, CORE.newOrdering(Grower.growerNoPropertyName, SortDirection.ascending));


//		int size = growers.size();
//		for (Grower grower : growers) {
//			log.add("    Processing grower " + grower.getGrowerNo());
//
//			process(grower, mailout, system, persistence, path.toString(), log);
//
//			growersProcessed++;
//
//			setPercentComplete((int) (((float) growersProcessed) / ((float) size) * 100F));
//		}

		setPercentComplete(100);
		log.add("Finished " + jobName + " at " + new Date());

		try {
			// create notification email
//			if (system.getFromEmailAddress() != null) {
//				String[] to = new String[] { system.getFromEmailAddress() };
//				EXT.sendMail(to, null, system.getFromEmailAddress(), "Job " + jobName + " - complete", "The job " + jobName + " has completed successfully.", MimeType.html, null, null, null);
//			}
			log.add("Notification sent");
		} catch (Exception e) {
			log.add("Notification failed " + e.getMessage());
		}

	}

}
