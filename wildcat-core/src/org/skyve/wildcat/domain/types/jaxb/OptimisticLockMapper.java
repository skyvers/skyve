package org.skyve.wildcat.domain.types.jaxb;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.skyve.domain.types.OptimisticLock;

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
