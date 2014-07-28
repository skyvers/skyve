package org.skyve.wildcat.content;

/**
 * Content has a path /customer/module/document/dataGroupId/userId/id (where customer is set in the workspace).
 * 
 * @author Mike
 */
public abstract class Content {
	private String path;
	protected String bizCustomer;
	protected String bizModule;
	protected String bizDocument;
	protected String bizDataGroupId;
	protected String bizUserId;
	protected String bizId;
	protected String attributeName;

	public String getPath() {
		if (path == null) {
			path = Content.createPathString(bizModule, bizDocument, bizDataGroupId, bizUserId, bizId, attributeName);
		}

		return path;
	}

	static String createPathString(String bizModule,
									String bizDocument,
									String bizDataGroupId,
									String bizUserId,
									String bizId,
									String attributeName) {
		StringBuilder sb = new StringBuilder(128);
		sb.append('/').append(bizModule).append('/');
		sb.append(bizDocument).append('/');
		sb.append(bizDataGroupId).append('/');
		sb.append(bizUserId).append('/');
		sb.append(bizId);
		if (attributeName != null) {
			sb.append('/').append(attributeName);
		}
		return sb.toString();
	}

	protected void setPath(String customerName, String path) {
		this.path = path;
		bizCustomer = customerName;
		String[] tokens = path.split("\\/");
		int length = tokens.length;
		if (length > 2) {
			bizModule = tokens[2];
		}
		if (length > 3) {
			bizDocument = tokens[3];
		}
		if (length > 4) {
			bizDataGroupId = tokens[4];
		}
		if ("null".equals(bizDataGroupId)) {
			bizDataGroupId = null;
		}
		if (length > 5) {
			bizUserId = tokens[5];
		}
		if (length > 6) {
			bizId = tokens[6];
		}
		if (length > 7) {
			attributeName = tokens[7];
		}
	}

	public String getBizCustomer() {
		return bizCustomer;
	}

	public String getBizDataGroupId() {
		return bizDataGroupId;
	}

	public String getBizDocument() {
		return bizDocument;
	}

	public String getBizModule() {
		return bizModule;
	}

	public String getBizUserId() {
		return bizUserId;
	}

	public String getBizId() {
		return bizId;
	}
}
