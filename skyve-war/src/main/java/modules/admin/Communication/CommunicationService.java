package modules.admin.Communication;

import org.skyve.domain.Bean;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.util.Util;

import jakarta.enterprise.inject.Default;
import modules.admin.domain.Communication;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient CommunicationService communicationService;
 */
@Default
public class CommunicationService {
	/**
	 * anonymously check whether a communication exists for a customer
	 * 
	 * Used in UnsubscribeView
	 * 
	 * @param pers
	 * @param bizCustomer
	 * @param communicationId
	 * @return
	 */
	@SuppressWarnings({ "boxing", "static-method" })
	public boolean anonymouslyCommunicationExists(Persistence persistence, String bizCustomer, String communicationId) {
		// temporarily elevate user to be able to see Communication records in case they don't usually have access
		Boolean result = persistence.withDocumentPermissionScopes(DocumentPermissionScope.customer, p -> {
			DocumentQuery q = p.newDocumentQuery(Communication.MODULE_NAME, Communication.DOCUMENT_NAME);
			q.addBoundProjection(Bean.DOCUMENT_ID);
			DocumentFilter f = q.getFilter();
			f.addEquals(Bean.CUSTOMER_NAME, bizCustomer);
			f.addEquals(Bean.DOCUMENT_ID, communicationId);
			return q.scalarResult(String.class) != null;
		});
		return Boolean.TRUE.equals(result);
	}

	@SuppressWarnings("static-method")
	public Communication setLinks(Communication communication) {
		Communication bean = communication;

		// construct the unsubscribe URL
		StringBuilder url = new StringBuilder(256);
		url.append(Util.getSkyveContextUrl());
		url.append("/");
		url.append("unsubscribe.xhtml?c=").append(bean.getBizCustomer());
		url.append("&i=").append(bean.getBizId());
		url.append("&r=").append(bean.getSendTo());
		bean.setUnsubscribeUrl(url.toString());

		return bean;
	}
}
