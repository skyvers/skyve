package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlSchemaType;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.types.jaxb.TimestampMapper;

/**
 * CorruptArchiveError
 * <br/>
 * Used as part of the recovery process for corrupt archives; 
		see org.skyve.impl.archive.job.ArchiveJob and its component jobs.
 * 
 * @depend - - - Resolution
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class CorruptArchiveError extends AbstractPersistentBean implements org.skyve.archive.support.CorruptArchiveError {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "CorruptArchiveError";

	/** @hidden */
	public static final String filenamePropertyName = "filename";

	/** @hidden */
	public static final String archiveTypeModulePropertyName = "archiveTypeModule";

	/** @hidden */
	public static final String archiveTypeDocumentPropertyName = "archiveTypeDocument";

	/** @hidden */
	public static final String timestampPropertyName = "timestamp";

	/** @hidden */
	public static final String resolutionPropertyName = "resolution";

	/**
	 * Archive Filename
	 **/
	private String filename;

	/**
	 * Archive Type Module
	 **/
	private String archiveTypeModule;

	/**
	 * Archive Type Document
	 **/
	private String archiveTypeDocument;

	/**
	 * Created Timestamp
	 **/
	private Timestamp timestamp;

	/**
	 * Resolution
	 **/
	private Resolution resolution;

	@Override
	@XmlTransient
	public String getBizModule() {
		return CorruptArchiveError.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return CorruptArchiveError.DOCUMENT_NAME;
	}

	public static CorruptArchiveError newInstance() {
		try {
			return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
		}
		catch (RuntimeException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage("Error {archiveTypeModule}.{archiveTypeDocument} - {filename}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #filename} accessor.
	 * @return	The value.
	 **/
	public String getFilename() {
		return filename;
	}

	/**
	 * {@link #filename} mutator.
	 * @param filename	The new value.
	 **/
	@XmlElement
	public void setFilename(String filename) {
		preset(filenamePropertyName, filename);
		this.filename = filename;
	}

	/**
	 * {@link #archiveTypeModule} accessor.
	 * @return	The value.
	 **/
	public String getArchiveTypeModule() {
		return archiveTypeModule;
	}

	/**
	 * {@link #archiveTypeModule} mutator.
	 * @param archiveTypeModule	The new value.
	 **/
	@XmlElement
	public void setArchiveTypeModule(String archiveTypeModule) {
		preset(archiveTypeModulePropertyName, archiveTypeModule);
		this.archiveTypeModule = archiveTypeModule;
	}

	/**
	 * {@link #archiveTypeDocument} accessor.
	 * @return	The value.
	 **/
	public String getArchiveTypeDocument() {
		return archiveTypeDocument;
	}

	/**
	 * {@link #archiveTypeDocument} mutator.
	 * @param archiveTypeDocument	The new value.
	 **/
	@XmlElement
	public void setArchiveTypeDocument(String archiveTypeDocument) {
		preset(archiveTypeDocumentPropertyName, archiveTypeDocument);
		this.archiveTypeDocument = archiveTypeDocument;
	}

	/**
	 * {@link #timestamp} accessor.
	 * @return	The value.
	 **/
	public Timestamp getTimestamp() {
		return timestamp;
	}

	/**
	 * {@link #timestamp} mutator.
	 * @param timestamp	The new value.
	 **/
	@XmlElement
	@XmlSchemaType(name = "dateTime")
	@XmlJavaTypeAdapter(TimestampMapper.class)
	public void setTimestamp(Timestamp timestamp) {
		preset(timestampPropertyName, timestamp);
		this.timestamp = timestamp;
	}

	/**
	 * {@link #resolution} accessor.
	 * @return	The value.
	 **/
	public Resolution getResolution() {
		return resolution;
	}

	/**
	 * {@link #resolution} mutator.
	 * @param resolution	The new value.
	 **/
	@XmlElement
	public void setResolution(Resolution resolution) {
		preset(resolutionPropertyName, resolution);
		this.resolution = resolution;
	}
}
