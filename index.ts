import * as pulumi from '@pulumi/pulumi';
import * as aws from '@pulumi/aws';
import * as awsx from '@pulumi/awsx';

// An ECS cluster to deploy into.
const cluster = new aws.ecs.Cluster('cluster', {});

// Create a load balancer to listen for requests and route them to the container.
const loadbalancer = new awsx.lb.ApplicationLoadBalancer('load-balancer', {});

const repo = new awsx.ecr.Repository('repo', {
	forceDelete: true,
});

const image = new awsx.ecr.Image('image', {
	repositoryUrl: repo.url,
	context: '.',
	imageName: 'haskeract',
	imageTag: 'latest',
	dockerfile: 'app/Dockerfile',
	args: {
		port: '80',
	},
});

const service = new awsx.ecs.FargateService('service', {
	cluster: cluster.arn,
	assignPublicIp: true,
	taskDefinitionArgs: {
		container: {
			name: 'haskeract',
			image: image.imageUri,
			cpu: 128,
			memory: 512,
			essential: true,
			portMappings: [
				{
					containerPort: 80,
					hostPort: 80,
					targetGroup: loadbalancer.defaultTargetGroup,
				},
			],
		},
	},
});

// Export the URL so we can easily access it.
export const frontendURL = pulumi.interpolate`http://${loadbalancer.loadBalancer.dnsName}`;
