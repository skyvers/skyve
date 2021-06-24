package org.skyve.domain.app.admin;

import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.admin.Communication.FormatType;

public interface Subscription extends PersistentBean {
	public static final String MODULE_NAME = "admin";
	public static final String DOCUMENT_NAME = "Subscription";

	public static final String receiverIdentifierPropertyName = "receiverIdentifier";
	public static final String formatTypePropertyName = "formatType";
	public static final String declinedPropertyName = "declined";
	
	Boolean getDeclined();
	String getPreferredReceiverIdentifier();
	FormatType getFormatType();
}
