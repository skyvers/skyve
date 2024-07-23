package org.skyve.impl.domain.types.jaxb;

import org.skyve.domain.types.OptimisticLock;

import jakarta.xml.bind.annotation.adapters.XmlAdapter;

//@XmlSchemaType(name = "string")
//@XmlJavaTypeAdapter(OptimisticLockMapper.class)
public class OptimisticLockMapper extends XmlAdapter<String, OptimisticLock> {
	@Override
	public OptimisticLock unmarshal(String lock) throws Exception {
		return (lock == null) ? null : new OptimisticLock(lock);
	}

	@Override
	public String marshal(OptimisticLock lock) throws Exception {
		return (lock == null) ? null : lock.toString();
	}
}
