package gov.nih.nci.caintegrator.analysis.server;

import org.apache.log4j.Logger;

import gov.nih.nci.caintegrator.analysis.messaging.AnalysisRequest;
import gov.nih.nci.caintegrator.analysis.messaging.AnalysisResult;
import gov.nih.nci.caintegrator.analysis.messaging.ClassComparisonLookupRequest;
import gov.nih.nci.caintegrator.analysis.messaging.ClassComparisonRequest;
import gov.nih.nci.caintegrator.analysis.messaging.CompoundAnalysisRequest;
import gov.nih.nci.caintegrator.analysis.messaging.CompoundAnalysisResult;
import gov.nih.nci.caintegrator.analysis.messaging.CopyNumberLookupRequest;
import gov.nih.nci.caintegrator.analysis.messaging.ExpressionLookupRequest;
import gov.nih.nci.caintegrator.analysis.messaging.FTestRequest;
import gov.nih.nci.caintegrator.analysis.messaging.GeneralizedLinearModelRequest;
import gov.nih.nci.caintegrator.analysis.messaging.HierarchicalClusteringRequest;
import gov.nih.nci.caintegrator.analysis.messaging.PrincipalComponentAnalysisRequest;
import gov.nih.nci.caintegrator.exceptions.AnalysisServerException;

public class CompoundRequestTaskR extends AnalysisTaskR {

	
	private CompoundAnalysisResult result = null;
	
	private static Logger logger = Logger.getLogger(CompoundRequestTaskR.class);

	
	
	public CompoundRequestTaskR(CompoundAnalysisRequest compoundRequest) {
		super(compoundRequest);
		
	}

	public CompoundRequestTaskR(CompoundAnalysisRequest compoundRequest, boolean debugRcommands) {
		super(compoundRequest, debugRcommands);
		
	}

	@Override
	public void run() {
		CompoundAnalysisRequest compoundRequest = (CompoundAnalysisRequest) getRequest();
		
		result = new CompoundAnalysisResult(compoundRequest.getSessionId(), compoundRequest.getTaskId());
		
		for (AnalysisRequest request : compoundRequest.getRequests()) {
			//execute the request
			logger.info("CompoundRequestTaskR: running request: " + request);
			runRequest(request);
			if (getException() != null) {
			  logger.error(getException());
			  return;
			}
		}
	}

	private void runRequest(AnalysisRequest request) {
		
	  //need to revisit the way we are mapping tasks to requests
		
	  AnalysisTaskR task = null;	
	  try {
		  if (request instanceof ClassComparisonLookupRequest) {
			task = new ClassComparisonLookupTaskR((ClassComparisonLookupRequest) request, getDebugRcommands());
		  }
		  else if (request instanceof ClassComparisonRequest) {
		    task = new ClassComparisonTaskR((ClassComparisonRequest) request, getDebugRcommands());
		  }
		  else if (request instanceof PrincipalComponentAnalysisRequest) {
		    task = new PrincipalComponentAnalysisTaskR((PrincipalComponentAnalysisRequest)request, getDebugRcommands());
		  }
		  else if (request instanceof HierarchicalClusteringRequest){
			task = new HierarchicalClusteringTaskR((HierarchicalClusteringRequest) request, getDebugRcommands());
		  }
		  else if (request instanceof FTestRequest){
			task = new FTestTaskR((FTestRequest)request, getDebugRcommands());
		  }
		  else if (request instanceof GeneralizedLinearModelRequest){
			task = new GeneralizedLinearModelTaskR((GeneralizedLinearModelRequest)request, getDebugRcommands());
		  }
		  else if (request instanceof ExpressionLookupRequest) {
			task = new ExpressionLookupTaskR((ExpressionLookupRequest) request, getDebugRcommands());
		  }
		  else if (request instanceof CopyNumberLookupRequest) {
			task = new CopyNumberLookupTaskR((CopyNumberLookupRequest) request, getDebugRcommands()); 
		  }
		  else {
			logger.error("Unrecognized request type :" + request.getClass());
		    throw new AnalysisServerException("Unrecognized request type");
		  }
		  task.setRComputeConnection(this.getRComputeConnection());
		  task.run();
		  if (task.getException() != null) {
		    throw task.getException();
		  }
		  result.addResult(task.getResult());
	  } catch (AnalysisServerException e) {
		
		  AnalysisServerException ex2 = new AnalysisServerException(e.getMessage());
		  ex2.setFailedRequest(getRequest());
		  this.setException(ex2);
		  
		  
	  } finally {
		task.cleanUp();
	  }
	}

	@Override
	public void cleanUp() {
		try {
			setRComputeConnection(null);
		} catch (AnalysisServerException e) {
			logger.error("Error in cleanUp method.");
			logStackTrace(logger, e);
			setException(e);
		}
	}

	@Override
	public AnalysisResult getResult() {
		return result;
	}

}
