package org.skyve.apps.interceptor;

import java.util.Map;
import java.util.TreeMap;

import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.util.BeanVisitor;
import org.skyve.wildcat.util.JSONUtil;

class AuditJSONGenerator extends BeanVisitor {
	private Map<String, Object> audit = new TreeMap<>();
	private Customer customer;
	
	AuditJSONGenerator(Customer customer) {
		this.customer = customer;
	}
	
	String toJSON() throws Exception {
		return JSONUtil.marshall(customer, audit, null);
	}

	@Override
	protected boolean accept(String binding,
								Document document,
								Document owningDocument,
								Reference owningReference,
								Bean bean,
								boolean visitingInheritedDocument)
	throws Exception {
		Map<String, Object> node = new TreeMap<>();

		for (Attribute attribute : document.getAttributes()) {
			if (! (attribute instanceof Relation)) {
				String name = attribute.getName();
				node.put(name, BindUtil.getSerialized(customer, bean, name));
			}
		}

		audit.put(binding, node);
		
		return true;
	}
}
