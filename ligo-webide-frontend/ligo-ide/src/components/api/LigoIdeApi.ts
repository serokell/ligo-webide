import { AxiosInstance, AxiosResponse } from "axios";

export interface CompileContractArgsApi {
  fileExtension: string;
  source: string;
}

export interface GenerateDeployScriptArgsApi {
  name: string;
  tzCode: string;
  storage: string;
}

interface LigoIdeApiInterface {
  compileContract(args: CompileContractArgsApi): Promise<string>;
  generateDeployScript(args: GenerateDeployScriptArgsApi): Promise<string>;
}

export default function LigoIdeApi(axiosInst: AxiosInstance): LigoIdeApiInterface {
  return {
    compileContract: async (args: CompileContractArgsApi) => {
      return axiosInst.post("/compile", args).then((resp: AxiosResponse<string>) => resp.data);
    },
    generateDeployScript: async (args: GenerateDeployScriptArgsApi) => {
      return axiosInst
        .post("/generate-deploy-script", args)
        .then((resp: AxiosResponse<string>) => resp.data);
    },
  };
}
