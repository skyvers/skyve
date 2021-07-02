package org.skyve.content;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.PersistentBean;

/**
 * Represents bean data stored in a content repository against the bean's bizId.
 * The bizId is the unique identifier used to manipulate this content.
 * @author mike
 */
public class BeanContent extends Content {
	private static final long serialVersionUID = 8279368686334430859L;

	/**
	 * Bean constructor.
	 * @param bean
	 */
	public BeanContent(PersistentBean bean) {
		super(bean.getBizCustomer(),
				bean.getBizModule(),
				bean.getBizDocument(),
				bean.getBizDataGroupId(),
				bean.getBizUserId(),
				bean.getBizId());
	}

	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Other bean properties that require indexing.
	 * @return
	 */
	public Map<String, String> getProperties() {
		return properties;
	}
}
