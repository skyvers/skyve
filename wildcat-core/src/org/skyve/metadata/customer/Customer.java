package org.skyve.metadata.customer;

import java.util.Collection;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.domain.types.converters.Converter;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.module.Module;

/**
 * 
 */
public interface Customer extends NamedMetaData {
	/**
	 * @return Returns the defaultDateConverter.
	 */
	public Converter<DateOnly> getDefaultDateConverter();

	/**
	 * @return Returns the defaultDateTimeConverter.
	 */
	public Converter<DateTime> getDefaultDateTimeConverter();
	
	/**
	 * @return Returns the defaultTimeConverter.
	 */
	public Converter<TimeOnly> getDefaultTimeConverter();
	
	/**
	 * @return Returns the defaultTimestampConverter.
	 */
	public Converter<Timestamp> getDefaultTimestampConverter();
	
	/**
	 * 
	 * @return
	 * @throws MetaDataException
	 */
	public Module getHomeModule() throws MetaDataException;
	
	/**
	 * 
	 * @param moduleName
	 * @return
	 * @throws MetaDataException
	 */
	public Module getModule(String moduleName) throws MetaDataException;
	
	/**
	 * 
	 * @return
	 * @throws MetaDataException
	 */
	public List<Module> getModules() throws MetaDataException;
	
	/**
	 * 
	 * @param serviceName
	 * @return
	 */
	public Service getService(String serviceName);
	
	/**
	 * 
	 * @return
	 */
	public Collection<Service> getServices();

	/**
	 * 
	 * @return
	 */
	public UIResources getUiResources();
	
	/**
	 * 
	 * @return
	 */
	public HTMLResources getHtmlResources();

	/**
	 * 
	 * @return
	 */
	public LoginResources getLoginResources();

	/**
	 * 
	 * @param bizlet
	 * @param documentName
	 * @param attribute
	 * @return
	 * @throws Exception
	 */
	public <T extends Bean> List<DomainValue> getConstantDomainValues(Bizlet<T> bizlet, 
																		String documentName, 
																		Attribute attribute)
	throws Exception;
}
