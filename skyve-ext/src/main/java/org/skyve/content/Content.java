package org.skyve.content;

import java.io.Serializable;

/**
 * Content.
 * 
 * @author Mike
 */
abstract class Content implements Serializable {
	private static final long serialVersionUID = -4098739421963456076L;

	protected String bizCustomer;
	protected String bizModule;
	protected String bizDocument;
	protected String bizDataGroupId;
	protected String bizUserId;
	protected String bizId;

	protected Content(String bizCustomer, 
						String bizModule,
						String bizDocument,
						String bizDataGroupId,
						String bizUserId,
						String bizId) {
		this.bizCustomer = bizCustomer;
		this.bizModule = bizModule;
		this.bizDocument = bizDocument;
		this.bizDataGroupId = bizDataGroupId;
		this.bizUserId = bizUserId;
		this.bizId = bizId;
	}

	public final String getBizCustomer() {
		return bizCustomer;
	}

	public final String getBizDataGroupId() {
		return bizDataGroupId;
	}

	public final String getBizDocument() {
		return bizDocument;
	}

	public final String getBizModule() {
		return bizModule;
	}

	public final String getBizUserId() {
		return bizUserId;
	}

	public final String getBizId() {
		return bizId;
	}
}
