import React, { useState } from "react";
import { Modal, DebouncedFormGroup } from "~/base-components/ui-components";
import fileOps from "~/base-components/file-ops";
import notification from "~/base-components/notification";
import Api from "~/components/api/api";

interface DeployScriptModalProps {
  modalRef: React.RefObject<Modal>;
  projectSettings: any;
  projectManager: any;
}

function DeployScriptModal({
  modalRef,
  projectSettings,
  projectManager,
}: DeployScriptModalProps): React.ReactElement | null {
  const [storage, setStorage] = useState<string>("");
  const [name, setName] = useState<string>("");
  const [loading, setLoading] = useState<boolean>(false);
  const [result, setResult] = useState<string>("");
  // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
  const tzFilePath: string = projectSettings?.get("deploy") || "";

  const onCreate = async () => {
    setLoading(true);

    const tzFile = await fileOps
      // eslint-disable-next-line @typescript-eslint/restrict-template-expressions, @typescript-eslint/no-unsafe-member-access
      .readFile(`${projectManager.projectRoot}/${tzFilePath.substring(2, tzFilePath.length)}`)
      .catch((e: Error) => {
        modalRef.current?.closeModal().catch((me: Error) => {
          console.error(me);
        });
        notification.error("Deploy Script Error", e.message);
      });

    setResult(tzFile || "");

    if (tzFile) {
      await Api.generateDeployScript({ name, tzCode: tzFile, storage })
        .then((resp) => setResult(resp))
        .catch((e: Error) => {
          modalRef.current?.closeModal().catch((me: Error) => {
            console.error(me);
          });
          notification.error("Deploy Script Error", e.message);
        });
    }
    setLoading(false);
  };

  return (
    <Modal
      ref={modalRef}
      title="Deploy Script"
      textConfirm="Generate"
      pending={loading && "Generating"}
      confirmDisabled={storage === "" || name === ""}
      onConfirm={onCreate}
      onCancel={() => {
        setLoading(false);
        setName("");
        setStorage("");
        setResult("");
        return true;
      }}
    >
      <DebouncedFormGroup
        label={
          <div>
            Generate deploy script for <kbd>{tzFilePath}</kbd> with init storage
          </div>
        }
        value={storage}
        onChange={(st: string) => setStorage(st)}
        placeholder="Storage"
      />
      <DebouncedFormGroup
        label={<div>Contract name in deploy script</div>}
        value={name}
        onChange={(n: string) => setName(n)}
        placeholder="Name"
      />
      {result && <div>{result}</div>}
    </Modal>
  );
}

export default DeployScriptModal;
