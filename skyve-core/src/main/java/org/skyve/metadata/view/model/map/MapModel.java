package org.skyve.metadata.view.model.map;

import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;
import org.skyve.metadata.MetaData;

public abstract class MapModel<T extends Bean> implements MetaData {
	private T bean;
	public T getBean() {
		return bean;
	}
	public void setBean(T bean) {
		this.bean = bean;
	}
	
	public abstract MapResult getResult(Geometry mapBounds) throws Exception;
}
